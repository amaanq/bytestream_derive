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
                    output.extend(quote! {
                        writeln!(f, "({}) {}", #strfy, self.#ident)?;
                    });
                } else {
                    let strfy = ident.as_ref().unwrap().to_string().to_pascal_case();
                    output.extend(quote! {
                        writeln!(f, "({}) {}", #strfy, self.#ident)?;
                    });
                }
            }

            (encoder, quote!(Self #reads), output)
        }
        Data::Enum(data) => {
            let endian = if find_attr(&item.attrs, "little_endian").is_some() {
                quote!(LittleEndian)
            } else {
                quote!(BigEndian)
            };

            let repr_attr = find_attr(&item.attrs, "repr")
                .ok_or_else(|| Error::new(item.span(), "Enum packets must declare #[repr]"))?;
            let vint_attr = find_attr(&item.attrs, "VInt").is_some();
            let repr_ty = repr_attr.parse_args::<Ident>()?;
            let (repr_write, _) = match repr_ty.to_string().as_str() {
                "u8" => (quote!(write_u8), quote!(read_u8)),
                "u16" => (
                    quote!(write_u16::<::byteorder::#endian>),
                    quote!(read_u16::<::byteorder::#endian>),
                ),
                "u32" => (
                    quote!(write_u32::<::byteorder::#endian>),
                    quote!(read_u32::<::byteorder::#endian>),
                ),
                "u64" => (
                    quote!(write_u64::<::byteorder::#endian>),
                    quote!(read_u64::<::byteorder::#endian>),
                ),
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
                    if #vint_attr {
                        ::bytestream_io::VInt(#discrim).encode(&mut w)?;
                    } else {
                        w.#repr_write(#discrim)?;
                    }
                    #fields_write
                }));
                read_vars.push(quote!(#discrim => #item_name::#var_name #fields_read));
                print_vars.push(quote!(#item_name::#var_name #fields_pat => {
                    #fields_print
                }));
            }

            let writer = quote! {{
                use ::byteorder::WriteBytesExt;
                match self {
                    #(#write_vars),*
                }
            }};

            let read_repr =
                if vint_attr { quote!(::bytestream_io::VInt) } else { quote!(#repr_ty) };

            let reader = quote! {{
                let id: #read_repr = ::bytestream_io::ByteStreamIo::decode(&mut r)?;
                match Into::<u32>::into(id) {
                    #(#read_vars,)*
                    _ => Err(::std::io::Error::new(::std::io::ErrorKind::Other, format!("Unexpected enum variant {:?}", id)))?,
                }
            }};
            let printer = quote! {{
                match self {
                    #(#print_vars),*
                }
            }};
            (writer, reader, printer)
        }
        Data::Union(_) => Err(Error::new(item.span(), "Unions cannot be derived as Packet"))?,
    };

    let ret = quote! {
        #[automatically_derived]
        impl ::bytestream_io::ByteStreamIo for #item_name {
            fn encode<W: ::std::io::Write>(&self, mut w: W) -> ::std::io::Result<()> {
                #encoder
                Ok(())
            }

            fn decode<R: ::std::io::Read>(mut r: R) -> ::std::io::Result<Self> {
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
                        quote! {writeln!(f, "({}) {}", #strfy, #ident)?;}
                    } else {
                        let strfy = ident.to_string().to_pascal_case();
                        quote! {writeln!(f, "({}) {}", #strfy, #ident)?;}
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
                    let ident = field.ident.as_ref().unwrap();
                    if ident.to_string().starts_with("unk") {
                        let strfy = quote!(#field.ty).to_string();
                        quote! {writeln!(f, "({}) {}", #strfy, #ident)?;}
                    } else {
                        let strfy = ident.to_string().to_pascal_case();
                        quote! { writeln!(f, "({}) {}", #strfy, #ident)?;}
                    }
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
        use ::bytestream_io::ByteStreamIo;
        (#expr).encode(&mut w)?;
    }}
}

fn read_field(ty: &Type) -> TokenStream {
    quote! {
        {
            let var: #ty = ::bytestream_io::ByteStreamIo::decode(&mut r)?;
            var
        }
    }
}

fn generate_ident(i: usize, span: Span) -> Ident {
    Ident::new(&format!("generated_ident_{}", i), span)
}
