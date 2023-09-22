use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_quote, DeriveInput, Expr, ExprLit, GenericParam, Meta};

fn from_inner_attr(field: &syn::Field) -> Option<proc_macro2::TokenStream> {
    let name = &field.ident;
    let dname = format!("{}", name.clone().unwrap());

    for attr in &field.attrs {
        assert!(attr.path().is_ident("debug"));
        let inner = if let Meta::NameValue(syn::MetaNameValue {
            value:
                Expr::Lit(ExprLit {
                    lit: syn::Lit::Str(custom_debug),
                    ..
                }),
            ..
        }) = &attr.meta
        {
            custom_debug
        } else {
            unimplemented!()
        };

        let val = &inner.value();
        return Some(quote! {
            field(#dname, &format_args!(#val, &self.#name))
        });
    }
    None
}

fn add_trait_bounds(mut generics: syn::Generics) -> syn::Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let dname = format!("{}", name);

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!("provided data type is not a struct");
    };

    let debug_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let dname = format!("{}", name.clone().unwrap());

        from_inner_attr(field).unwrap_or_else(|| {
            quote! {
                field(#dname, &self.#name)
            }
        })
    });

    // Add a bound `T: Debug` to every type parameter T.
    let generics = add_trait_bounds(ast.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#dname)
                  #(.#debug_fields)*
                 .finish()
            }
        }
    }
    .into()
}
