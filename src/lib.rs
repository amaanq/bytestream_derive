extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod bytestream;

/// Generate `ByteStream` implementation for structs and enums that have all fields
/// implement `ByteStream`.
///
/// For structs, fields are written one by one in order.
///
/// For enums, the structure starts with a discriminant with the type specified in the `#[repr]` of
/// the enum, followed by the fields of the enum one by one. If the enum repr should be little
/// endian, the `#[little_endian]` attribute must be applied on the `enum` item. If the enum repr
/// should be encoded as a `VInt`, the `#[VInt]` attribute must be applied on the `enum` item.
#[proc_macro_derive(ByteStream, attributes(little_endian, VInt))]
pub fn derive_message(item: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(item as DeriveInput);
    match bytestream::imp(&parsed) {
        Ok(item) => item.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
