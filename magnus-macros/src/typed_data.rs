use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    spanned::Spanned, Data, DataEnum, DataStruct, DeriveInput, Error, Fields, FieldsNamed, LitStr,
};

use crate::util;

pub fn expand(attrs: TokenStream, item: TokenStream) -> TokenStream {
    quote! {
        #[derive(magnus::DataTypeFunctions, magnus::TypedData)]
        #[magnus(#attrs)]
        #item
    }
}

pub fn expand_derive_data_type_functions(input: DeriveInput) -> TokenStream {
    let ident = input.ident;
    let generics = input.generics;
    quote! {
        impl #generics magnus::DataTypeFunctions for #ident #generics {}
    }
}

pub fn expand_derive_typed_data(input: DeriveInput) -> Result<TokenStream, Error> {
    let attrs = match util::get_magnus_attribute(&input.attrs)? {
        Some(v) => v,
        None => return Err(Error::new(input.span(), "missing #[magnus] attribute")),
    };

    let mut class = None;
    let mut name = None;
    let mut mark = false;
    let mut size = false;
    let mut compact = false;
    let mut free_immediately = false;
    let mut wb_protected = false;
    let mut frozen_shareable = false;
    let mut unsafe_generics = false;
    let mut accessors = false;
    let mut bindings = false;

    attrs.parse_nested_meta(|meta| {
        if meta.path.is_ident("class") {
            class = Some(meta.value()?.parse::<LitStr>()?.value());
            Ok(())
        } else if meta.path.is_ident("name") {
            name = Some(meta.value()?.parse::<LitStr>()?.value());
            Ok(())
        } else if meta.path.is_ident("mark") {
            mark = true;
            Ok(())
        } else if meta.path.is_ident("size") {
            size = true;
            Ok(())
        } else if meta.path.is_ident("compact") {
            compact = true;
            Ok(())
        } else if meta.path.is_ident("free_immediately") {
            free_immediately = true;
            Ok(())
        } else if meta.path.is_ident("wb_protected") {
            wb_protected = true;
            Ok(())
        } else if meta.path.is_ident("frozen_shareable") {
            frozen_shareable = true;
            Ok(())
        } else if meta.path.is_ident("unsafe_generics") {
            unsafe_generics = true;
            Ok(())
        } else if meta.path.is_ident("accessors") {
            accessors = true;
            Ok(())
        } else if meta.path.is_ident("bindings") {
            bindings = true;
            Ok(())
        } else if meta.path.is_ident("free_immediatly") {
            Err(meta.error("unsupported attribute (use free_immediately)"))
        } else {
            Err(meta.error("unsupported attribute"))
        }
    })?;

    if !input.generics.to_token_stream().is_empty() && !unsafe_generics {
        let case = if input.generics.type_params().count() > 0 {
            "containing generic types"
        } else if input.generics.lifetimes().count() > 0 {
            "with lifetimes"
        } else if input.generics.const_params().count() > 0 {
            "with const generics"
        } else {
            "containing generic types"
        };
        return Err(Error::new_spanned(
            input.generics,
            format!("deriving TypedData is not guaranteed to be correct for types {}, consider removing them, or use `#[magnus(unsafe_generics)]` to override this error.", case),
        ));
    }

    let class = match class {
        Some(v) => v,
        None => return Err(Error::new(attrs.span(), "missing attribute: `class = ...`")),
    };
    let name = name.unwrap_or_else(|| class.clone());

    let ident = &input.ident;
    let generics = &input.generics;

    let mut arms = Vec::new();
    if let Data::Enum(DataEnum { ref variants, .. }) = input.data {
        for variant in variants.into_iter() {
            let attrs = match util::get_magnus_attribute(&variant.attrs)? {
                Some(v) => v,
                None => continue,
            };
            let mut class = None;
            attrs.parse_nested_meta(|meta| {
                if meta.path.is_ident("class") {
                    class = Some(meta.value()?.parse::<LitStr>()?.value());
                    Ok(())
                } else {
                    Err(meta.error("unsupported attribute"))
                }
            })?;
            let class = match class {
                Some(v) => v,
                None => return Err(Error::new(attrs.span(), "missing attribute: `class = ...`")),
            };
            let ident = &variant.ident;
            let fetch_class = quote! {
                static CLASS: Lazy<RClass> = Lazy::new(|ruby| {
                    let class: RClass = ruby.class_object().funcall("const_get", (#class,)).unwrap();
                    class.undef_default_alloc_func();
                    class
                });
                ruby.get_inner(&CLASS)
            };
            arms.push(match variant.fields {
                Fields::Named(_) => quote! { Self::#ident { .. } => { #fetch_class } },
                Fields::Unnamed(_) => quote! { Self::#ident(_) => { #fetch_class } },
                Fields::Unit => quote! { Self::#ident => #fetch_class },
            });
        }
    }
    let class_for = if !arms.is_empty() {
        quote! {
            fn class_for(ruby: &magnus::Ruby, value: &Self) -> magnus::RClass {
                use magnus::{class, Module, Class, RClass, value::{Lazy, ReprValue}};
                #[allow(unreachable_patterns)]
                match value {
                    #(#arms,)*
                    _ => Self::class(ruby),
                }
            }
        }
    } else {
        quote! {}
    };

    let mut accessor_fns = Vec::new();
    if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        for field in named {
            let mut opaque_attr_reader = false;
            if let Some(attrs) = util::get_magnus_attribute(&field.attrs)? {
                attrs.parse_nested_meta(|meta| {
                    if meta.path.is_ident("opaque_attr_reader") {
                        opaque_attr_reader = true;
                        Ok(())
                    } else {
                        Err(meta.error("unsupported attribute"))
                    }
                })?;
            };
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;
            if opaque_attr_reader {
                accessor_fns.push(quote! {
                    #[inline]
                    fn #ident(&self) -> <#ty as magnus::value::OpaqueVal>::Val {
                        let ruby = unsafe { magnus::Ruby::get_unchecked() };
                        ruby.get_inner(self.#ident)
                    }
                });
            } else if accessors {
                accessor_fns.push(match quote! { #ty }.to_string().as_str() {
                    ty if ty.starts_with("Vec") => quote! {
                        #[inline]
                        fn #ident(&self) -> magnus::RArray {
                            let ruby = unsafe { magnus::Ruby::get_unchecked() };
                            ruby.ary_from_vec(self.#ident.clone())
                        }
                    },
                    "Uuid" => quote! {
                        #[inline]
                        fn #ident(&self) -> String {
                            self.#ident.to_string()
                        }
                    },
                    _ => quote! {
                        #[inline]
                        fn #ident(&self) -> #ty {
                            self.#ident.clone()
                        }
                    }
                });
            }
        }
    }
    let accessor_impl = quote! {
        impl #ident {
            #(#accessor_fns)*
        }
    };

    let bindings_impl = if bindings {
        let mut modules: Vec<_> = class.split("::").collect();
        let class = modules.pop().unwrap();
        let modules: Vec<_> = modules
            .iter()
            .enumerate()
            .map(|(i, m)| {
                let var = if i == 0 {
                    quote! { ruby }
                } else {
                    quote! { module }
                };
                quote! { let module = #var.define_module(#m)?; }
            })
            .collect();
        let mut accessor_bindings = Vec::new();
        if let Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { ref named, .. }),
            ..
        }) = input.data
        {
            for field in named {
                let name = field.ident.as_ref().unwrap();
                accessor_bindings.push(quote! {
                    class.define_method(stringify!(#name), magnus::method!(#ident::#name, 0))?;
                });
            }
        }
        quote! {
            use magnus::Module as _;
            impl #ident {
                pub fn bindings(ruby: &magnus::Ruby) -> Result<(), magnus::Error> {
                    #(#modules)*
                    let class = module.define_class(#class, ruby.class_object())?;
                    #(#accessor_bindings)*
                    Ok(())
                }
            }
        }
    } else {
        quote! {}
    };

    let mut builder = Vec::new();
    builder.push(quote! { magnus::data_type_builder!(#ident, #name) });
    if mark {
        builder.push(quote! { .mark() });
    }
    if size {
        builder.push(quote! { .size() });
    }
    if compact {
        builder.push(quote! { .compact() });
    }
    if free_immediately {
        builder.push(quote! { .free_immediately() });
    }
    if wb_protected {
        builder.push(quote! { .wb_protected() });
    }
    if frozen_shareable {
        builder.push(quote! { .frozen_shareable() });
    }
    builder.push(quote! { .build() });
    let builder = builder.into_iter().collect::<TokenStream>();
    let tokens = quote! {
        #accessor_impl
        #bindings_impl

        unsafe impl #generics magnus::TypedData for #ident #generics {
            fn class(ruby: &magnus::Ruby) -> magnus::RClass {
                use magnus::{class, Module, Class, RClass, value::{Lazy, ReprValue}};
                static CLASS: Lazy<RClass> = Lazy::new(|ruby| {
                    let class: RClass = ruby.class_object().funcall("const_get", (#class,)).unwrap();
                    class.undef_default_alloc_func();
                    class
                });
                ruby.get_inner(&CLASS)
            }

            fn data_type() -> &'static magnus::DataType {
                static DATA_TYPE: magnus::DataType = #builder;
                &DATA_TYPE
            }

            #class_for
        }
    };
    Ok(tokens)
}
