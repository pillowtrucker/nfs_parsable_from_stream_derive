#![crate_type = "proc-macro"]
#![feature(log_syntax)]
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn;
use syn::Fields;

#[proc_macro_derive(NFSParsable)]
pub fn hello_macro_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_nfsparsable_macro(ast)
}

fn impl_nfsparsable_macro(ast: syn::DeriveInput) -> TokenStream {
    
    let name = &ast.ident;
    let generics = ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let ty_lt = match generics.lifetimes().into_iter().peekable().peek() {
        Some(_) => quote!(<'_>),
        None => quote!()
};
    let data: syn::Data = ast.data;
//    let actual_return_type_from_generics = generics.type_params().next().unwrap();
    let mut inner = quote!();
    let mut first = true;
    
    let mut fnames = quote!();
    if let Some(fields) = match &data {
        syn::Data::Struct(inner) => Some(&inner.fields),
        syn::Data::Enum(inner) => None,
        syn::Data::Union(inner) => None
    } {
        match fields {

            Fields::Named(named) => for field in named.named.iter() {

                let fname = &field.ident.as_ref().unwrap();
                let ftype = &field.ty;
                let disambiguate_generic_field_implementation = match ftype {
                    syn::Type::Path(p) => {match p.path.segments.clone().into_iter().next().unwrap().arguments{
                        syn::PathArguments::AngleBracketed(yes) => quote!(::#yes),
                        _ => quote!()

                    }},
                    _ => quote!()
                        };

                if let Some(no_generic_garbage) = match ftype {
                    syn::Type::Path(p) => Some(p.path.segments.clone().into_iter().next().unwrap().ident),
                    _ => None
                } {
                    fnames.extend(quote!(#fname,));
                    
                    if first {
                        inner.extend(quote!(
                            let (rest, #fname) = #no_generic_garbage#disambiguate_generic_field_implementation::from_stream(data)?;
                        ));
                        first = false;
                    } else {
                        inner.extend(quote!(
                            let (rest, #fname) = #no_generic_garbage#disambiguate_generic_field_implementation::from_stream(rest)?;
                        ));
                    }
                }

    }
            _ => unimplemented!()
}

    }
    let thereturn = quote!(
        Ok((rest, #name {#fnames}))
    );
    if let Some(variants) = match &data {
        syn::Data::Struct(inner) => None,
        syn::Data::Enum(inner) => Some(&inner.variants),
        syn::Data::Union(inner) => None
    } {
        

    }
    let outer = quote! {
        impl<'a> NFSParsable<'a,#name#ty_lt> for #name#ty_lt {
//            fn from_stream(data: &'a[u8]) -> IResult<&'a[u8], #actual_return_type_from_generics> {
            fn from_stream(data: &'a[u8]) -> IResult<&'a[u8], #name#ty_lt> {
                #inner
                #thereturn
            }
        }               
};
    
    outer.into()



        

 
}
