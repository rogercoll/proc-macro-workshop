use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, Expr, Ident, Lit};

/// ty_inner_type returns the inner type of types wrapped inside AngleBracketeds
fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                // eprintln!("Innery type: {:?}", t);
                return Some(t);
            }
        }
    }
    None
}

/*
    #[builder(each = "arg")]
    Extracts arg value from builder attribute
*/
fn builder_of(field: &syn::Field) -> Option<Ident> {
    // return Some("arg".into());
    for attr in &field.attrs {
        if attr.path().is_ident("builder") {
            let precondition: Expr = attr.parse_args().unwrap();
            if let Expr::Assign(assing) = precondition {
                if let Expr::Path(left) = *assing.left {
                    // assert valid attribute

                    assert!(
                        left.path.segments.len() == 1
                            && left.path.segments[0].ident.to_string() == "each"
                    );
                } else {
                    return None;
                }
                if let Expr::Lit(fn_name) = *assing.right {
                    if let Lit::Str(parsed_fn_name) = fn_name.lit {
                        return Some(Ident::new(&parsed_fn_name.value(), parsed_fn_name.span()));
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);
    // eprint!("{:#?}", ast);

    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(&bname, name.span());

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!("provided data type is not a struct");
    };

    let optionized = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;

        if ty_inner_type("Option", &field.ty).is_some() || builder_of(&field).is_some() {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: std::option::Option<#ty>
            }
        }
    });

    let methods = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;

        let with_builder_attr = builder_of(&field);

        if let Some(inner_ty) = ty_inner_type("Option", &ty) {
            quote! {
                fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else if with_builder_attr.is_some() {
            let inner_ty = ty_inner_type("Vec", &ty).unwrap();
            let fn_name = with_builder_attr.unwrap();
            if fn_name == name.clone().unwrap().to_string() {
                quote! {
                    fn #fn_name(&mut self, #name: #inner_ty) -> &mut Self {
                        self.#name.push(#name);
                        self
                    }
                }
            } else {
                quote! {
                    fn #fn_name(&mut self, #name: #inner_ty) -> &mut Self {
                        self.#name.push(#name);
                        self
                    }
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = #name;
                        self
                    }
                }
            }
        } else {
            quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        if ty_inner_type("Option", &field.ty).is_some() || builder_of(&field).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name)," is not set"))?
            }
        }
    });

    let build_empty = fields.iter().map(|field| {
        let name = &field.ident;
        if builder_of(field).is_some() {
            quote! { #name: std::vec::Vec::new() }
        } else {
            quote! { #name: None }
        }
    });

    let expanded = quote! {
        pub struct #bident {
            #(#optionized,)*
        }
        impl #bident {
            #(#methods)*
            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                        #(#build_fields,)*
                    }
                )

            }

        }

        impl #name {
            fn builder() -> #bident {
                #bident {
                    #(#build_empty,)*
                }
            }
        }
    };
    expanded.into()
}
