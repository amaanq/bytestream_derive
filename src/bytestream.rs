use heck::ToPascalCase;
use proc_macro2::{Literal, Span, TokenStream};
use quote::quote;
use syn::spanned::Spanned;
use syn::{Attribute, Data, DeriveInput, Error, Fields, Ident, Result, Type};

pub fn imp(item: &DeriveInput) -> Result<TokenStream> {
    let item_name = &item.ident;

    let (encoder, decoder, output) = match &item.data {
        Data::Struct(data) => {
            let encoder = write_fields(
                &data.fields,
                |ident| quote!(self.#ident),
                |i, _| {
                    let i = Literal::usize_unsuffixed(i);
                    quote!(self.#i)
                },
            );

            let reads = read_fields(&data.fields);

            let mut output = quote!();
            for field in &data.fields {
                let ident = &field.ident;
                let ty = &field.ty;
                if ident.as_ref().unwrap().to_string().starts_with("unk") {
                    let strfy = quote!(#ty).to_string();
                    if strfy.starts_with("Option") && strfy.ends_with('>') && strfy.contains('<') {
                        let inner = strfy[7..strfy.len() - 1].to_string();
                        output.extend(quote! {
                            writeln!(f, "({}) {:?}", #inner, self.#ident)?;
                        });
                    } else {
                        output.extend(quote! {
                            writeln!(f, "({}) {}", #strfy, self.#ident)?;
                        });
                    }
                } else {
                    let strfy = ident.as_ref().unwrap().to_string().to_pascal_case();
                    let ty_name = quote!(#ty).to_string();
                    if ty_name.starts_with("Option")
                        && ty_name.ends_with('>')
                        && ty_name.contains('<')
                    {
                        output.extend(quote! {
                            writeln!(f, "({}) {:?}", #strfy, self.#ident)?;
                        });
                    } else {
                        output.extend(quote! {
                            writeln!(f, "({}) {}", #strfy, self.#ident)?;
                        });
                    }
                }
            }

            (encoder, quote!(Self #reads), output)
        }
        Data::Enum(data) => {
            let repr_attr = find_attr(&item.attrs, "repr")
                .ok_or_else(|| Error::new(item.span(), "Enum packets must declare #[repr]"))?;
            let repr_ty = repr_attr.parse_args::<Ident>()?;
            let (repr_write, _) = match repr_ty.to_string().as_str() {
                "u8" => (quote!(write_byte), quote!(read_byte)),
                "u16" => (quote!(write_u16), quote!(read_u16)),
                "u32" => (quote!(write_int), quote!(read_int)),
                "u64" => (quote!(write_u64), quote!(read_u64)),
                _ => Err(Error::new(
                    repr_attr.tokens.span(),
                    "Only repr(u[8|16|32|64]) enums are supported",
                ))?,
            };

            let mut write_vars = Vec::with_capacity(data.variants.len());
            let mut read_vars = Vec::with_capacity(data.variants.len());
            let mut print_vars = Vec::with_capacity(data.variants.len());
            for variant in &data.variants {
                let var_name = &variant.ident;
                let (_, discrim) = variant.discriminant.as_ref().ok_or_else(|| {
                    Error::new(variant.span(), "All enum packet variants must have discriminants")
                })?;
                let fields_pat = pat_fields(&variant.fields);

                let fields_write = write_fields(
                    &variant.fields,
                    |ident| quote!(#ident),
                    |id, span| {
                        let ident = generate_ident(id, span);
                        quote!(#ident)
                    },
                );
                let fields_read = read_fields(&variant.fields);
                let fields_print = print_fields(&variant.fields);

                write_vars.push(quote!(#item_name::#var_name #fields_pat => {
                    encoder.#repr_write(#discrim);
                    #fields_write
                }));
                read_vars.push(quote!(#discrim => #item_name::#var_name #fields_read));
                print_vars.push(quote!(#item_name::#var_name #fields_pat => {
                    #fields_print
                }));
            }

            let encoder = quote! {{
                match self {
                    #(#write_vars),*
                }
            }};

            let read_repr = quote!(#repr_ty);

            let decoder = quote! {{
                let id: #read_repr = ::titan::ByteStreamIo::decode(stream)?;
                match id {
                    #(#read_vars,)*
                    _ => {
                        tracing::warn!("Unexpected enum variant {} {:?}", stringify!(#item_name), id);
                        #item_name::default()
                    }
                }
            }};
            let output = quote! {{
                match self {
                    #(#print_vars),*
                }
            }};
            (encoder, decoder, output)
        }
        // Data::Union(_) => Err(Error::new(item.span(), "Unions cannot be derived as Packet"))?,
        Data::Union(_) => panic!("Unions cannot be derived as Packet"),
    };

    let ret = quote! {
        #[automatically_derived]
        impl ::titan::ByteStreamIo for #item_name {
            fn encode(&self, encoder: &mut dyn ::titan::ChecksumEncoder) {
                #encoder
            }

            fn decode(stream: &mut ::titan::ByteStream) -> ::titan::ByteStreamResult<Self> {
                Ok(#decoder)
            }
        }

        impl std::fmt::Display for #item_name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                #output
                Ok(())
            }
        }
    };
    Ok(ret)
}

fn find_attr<'a, I, S>(attr: I, name: S) -> Option<&'a Attribute>
where
    I: IntoIterator<Item = &'a Attribute>,
    S: AsRef<str>,
{
    attr.into_iter().find(|attr| attr.path.is_ident(&name))
}

fn write_fields<F, G>(fields: &Fields, access_named: F, access_unnamed: G) -> TokenStream
where
    F: Fn(&Ident) -> TokenStream,
    G: Fn(usize, Span) -> TokenStream,
{
    match fields {
        Fields::Named(fields) => {
            let fields: Vec<TokenStream> = fields
                .named
                .iter()
                .map(|field| {
                    let ident = field.ident.as_ref().unwrap();
                    let accessor = access_named(ident);
                    write_field(&field.ty, &accessor)
                })
                .collect();
            quote!(#(#fields)*)
        }
        Fields::Unnamed(fields) => {
            let fields: Vec<TokenStream> = fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, field)| {
                    let accessor = access_unnamed(i, field.span());
                    write_field(&field.ty, &accessor)
                })
                .collect();
            quote!(#(#fields)*)
        }
        Fields::Unit => quote!(),
    }
}

fn read_fields(fields: &Fields) -> TokenStream {
    match fields {
        Fields::Named(fields) => {
            let fields: Vec<TokenStream> = fields
                .named
                .iter()
                .map(|field| {
                    let field_name = &field.ident;
                    let read_expr = read_field(&field.ty);
                    quote!(#field_name: #read_expr)
                })
                .collect();
            quote!({ #(#fields),* })
        }
        Fields::Unnamed(fields) => {
            let fields: Vec<TokenStream> = fields
                .unnamed
                .iter()
                .map(|field| {
                    let read_expr = read_field(&field.ty);
                    quote!(#read_expr)
                })
                .collect();
            quote!(( #(#fields),* ))
        }
        Fields::Unit => quote!(),
    }
}

fn print_fields(fields: &Fields) -> TokenStream {
    match fields {
        Fields::Named(fields) => {
            let fields: Vec<TokenStream> = fields
                .named
                .iter()
                .map(|field| {
                    let ident = field.ident.as_ref().unwrap();
                    // let accessor = access_named(ident);
                    if ident.to_string().starts_with("unk") {
                        let strfy = quote!(#field.ty).to_string();
                        // check if field.ty is an option<t>
                        if strfy.starts_with("Option")
                            && strfy.ends_with('>')
                            && strfy.contains('<')
                        {
                            let inner = strfy[7..strfy.len() - 1].to_string();
                            quote! {writeln!(f, "({}) {:?}", #inner, #ident)?;}
                        } else {
                            quote! {writeln!(f, "({}) {}", #strfy, #ident)?;}
                        }
                    } else {
                        let strfy = ident.to_string().to_pascal_case();
                        let ty = quote!(#field.ty).to_string();
                        if ty.starts_with("Option") && ty.ends_with('>') && ty.contains('<') {
                            let inner = ty[7..strfy.len() - 1].to_string();
                            quote! {writeln!(f, "({}) {:?}", #inner, #ident)?;}
                        } else {
                            quote! {writeln!(f, "({}) {}", #strfy, #ident)?;}
                        }
                    }
                })
                .collect();
            quote!(#(#fields)*)
        }
        Fields::Unnamed(fields) => {
            let fields: Vec<TokenStream> = fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(_, field)| {
                    // let accessor = access_unnamed(i, field.span());
                    // let ident = field.ident.as_ref().unwrap();
                    // if ident.to_string().starts_with("unk") {
                    //     let strfy = quote!(#field.ty).to_string();
                    //     quote! {writeln!(f, "({}) {}", #strfy, #ident)?;}
                    // } else {
                    //     let strfy = ident.to_string().to_pascal_case();
                    //     quote! { writeln!(f, "({}) {}", #strfy, #ident)?;}
                    // }
                    let strfy = quote!(#field.ty).to_string();
                    quote! {writeln!(f, "({}) {}", #strfy, #field)?;}
                })
                .collect();
            quote!(#(#fields)*)
        }
        Fields::Unit => quote!(),
    }
}

fn pat_fields(fields: &Fields) -> TokenStream {
    match fields {
        Fields::Named(fields) => {
            let fields = fields.named.iter().map(|field| field.ident.as_ref().unwrap());
            quote!({ #(#fields),* })
        }
        Fields::Unnamed(fields) => {
            let fields =
                fields.unnamed.iter().enumerate().map(|(i, field)| generate_ident(i, field.span()));
            quote!(( #(#fields),* ))
        }
        Fields::Unit => quote!(),
    }
}

fn write_field(_ty: &Type, expr: &TokenStream) -> TokenStream {
    quote! {{
        use ::titan::ChecksumEncoder;
        (#expr).encode(encoder);
    }}
}

fn read_field(ty: &Type) -> TokenStream {
    quote! {
        {
            let var: #ty = ::titan::ByteStreamIo::decode(stream)?;
            var
        }
    }
}

fn generate_ident(i: usize, span: Span) -> Ident {
    Ident::new(&format!("generated_ident_{i}"), span)
}
