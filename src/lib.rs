extern crate proc_macro;

use darling::{FromDeriveInput, FromField, FromVariant};
use itertools::Itertools;
use quote::{format_ident, quote};
use proc_macro2::TokenStream as TokenStream2;
use syn::{parse_macro_input, DeriveInput};


#[derive(Debug, FromDeriveInput)]
#[darling(attributes(packer), supports(struct_unit, struct_newtype, struct_tuple, enum_newtype))]
struct DarlingRuleAttr {
    ident: syn::Ident,
    rule: syn::Path,
    data: darling::ast::Data<DarlingEnumVariant, DarlingTupleStructField>
}

#[derive(Debug, FromVariant)]
struct DarlingEnumVariant {
    ident: syn::Ident,
    fields: darling::ast::Fields<syn::Type>,
}

#[derive(Debug, FromField)]
struct DarlingTupleStructField {
    ty: syn::Type,
}

enum PackDataStructure {
    UnitStruct,
    TupleStruct(PackTupleStruct),
    Enum(PackEnumNewtype),
}

struct PackEnumNewtype(Vec<NameTypePair>);

struct NameTypePair(syn::Ident, PackerType);

struct PackTupleStruct(Vec<PackerType>);

#[derive(Debug)]
enum PackerType {
    Usize,
    String,
    NaiveTime,
    Option,
    Vec,
    UserType(syn::Ident),
    Box,
}

macro_rules! try_match {
    ($val:expr, $pattern:pat => $transform:expr) => {
        match $val {
            $pattern => Some($transform),
            _ => None,
        }
    };
}

fn parse_packer_type(ty: syn::Type) -> Option<PackerType> {
    let path = try_match!(ty, syn::Type::Path(path) => path).expect("A").path;
    
    let packer_type = match path.segments.first().expect("B").ident.to_string().as_str() {
        "usize" => PackerType::Usize,
        "String" => PackerType::String,
        "NaiveTime" => PackerType::NaiveTime,
        "Option" => PackerType::Option,
        "Vec" => PackerType::Vec,
        "Box" => PackerType::Box,
        _ => PackerType::UserType(path.get_ident().expect("K").clone()),
    };

    Some(packer_type)
}

impl Into<PackDataStructure> for darling::ast::Data<DarlingEnumVariant, DarlingTupleStructField> {
    fn into(self) -> PackDataStructure {
        if self.is_struct() {
            let types = self.take_struct().expect("Could not take struct")
                .fields.into_iter()
                .map(|f| parse_packer_type(f.ty))
                .collect::<Option<Vec<_>>>()
                .expect("I got confused");

            if types.is_empty() {
                PackDataStructure::UnitStruct
            } else {
                PackDataStructure::TupleStruct(PackTupleStruct(types))
            }
        } else {
            let enum_variants = self.take_enum().expect("Could not take enum")
                .into_iter()
                .map(|variant| {
                    let field = variant.fields
                        .fields.into_iter()
                        .exactly_one().expect("Could not take exactly one field");

                    NameTypePair(variant.ident, parse_packer_type(field).expect("aaaAAAaahh"))
                })
                .collect();

            PackDataStructure::Enum(PackEnumNewtype(enum_variants))
        }
    }
}

fn expand_enum(structure: PackEnumNewtype, debug: bool) -> Result<TokenStream2, darling::Error> {
    let PackEnumNewtype(pairs) = structure;

    let match_arms = pairs.iter()
        .map(|pair| {
            let NameTypePair(name, packer) = pair;
            let packer = try_match!(packer, PackerType::UserType(user_type) => user_type).expect("Should've been user type");

            quote! {
                t if #packer::is_packable(&t) =>
                    #packer::pack(t)
                        .map(Self::#name),
            }
        })
        .collect_vec();

    let expected_rules = pairs.iter()
        .map(|pair| {
            let NameTypePair(_, packer) = pair;
            let packer = try_match!(packer, PackerType::UserType(user_type) => user_type).expect("Should've been user type");

            quote! {
                <#packer as lang_packer_model::pack_trees::HasRule>::get_rule(),
            }
        })
        .collect_vec();

    let output = quote! {
        match lang_packer_model::pack_trees::get_only_tree_child(tree, <Self as lang_packer_model::pack_trees::HasRule>::get_rule())? {
            #(#match_arms)*
            
            t => {
                let expected_rules = vec![
                    #(#expected_rules)*
                ];

                Err(lang_packer_model::pack_trees::make_wrong_rules_alt_error(t, expected_rules))
            }
        }
    };

    Ok(output)
}

fn expand_unit_struct(debug: bool) -> Result<TokenStream2, darling::Error> {
    let maybe_debug_line = if debug {
        quote! { println!("expand_unit_struct({})", <Self as lang_packer_model::pack_trees::HasRule>::get_rule()); }
    } else { quote! { } };

    let packer = quote! {
        lang_packer_model::pack_trees::get_tree_src_string(tree, <Self as lang_packer_model::pack_trees::HasRule>::get_rule())?;

        Ok(Self)
    };

    Ok(quote!{ { #maybe_debug_line #packer } })
}

fn get_packer_snippet(packer_type: PackerType, debug: bool) -> Result<TokenStream2, darling::Error> {
    let maybe_debug_line = if debug {
        let str = format!("{packer_type:?}");

        quote! { println!("get_packer_snippet({})", #str); }
    } else { quote! { } };

    let tokens = match packer_type {
        PackerType::Vec => quote! {
            lang_packer_model::pack_trees::get_vec_of_packer(&mut iter)
        },
        PackerType::Usize => quote! {
            lang_packer_model::pack_trees::get_next_tree(&mut iter)
                .and_then(lang_packer_model::pack_trees::get_usize_from_tree)
        },
        PackerType::String => quote! {
            lang_packer_model::pack_trees::get_next_tree(&mut iter)
                .and_then(lang_packer_model::pack_trees::get_string_from_tree)
        },
        PackerType::NaiveTime => quote! {
            lang_packer_model::pack_trees::get_next_tree(&mut iter)
                .and_then(lang_packer_model::pack_trees::get_naive_time_from_tree)
        },
        PackerType::Option => quote! {
            lang_packer_model::pack_trees::maybe_pack_next_tree(&mut iter)
        },
        PackerType::Box => quote! {
            lang_packer_model::pack_trees::pack_next_tree(&mut iter).map(Box::new)
        },
        PackerType::UserType(_) => quote! {
            lang_packer_model::pack_trees::pack_next_tree(&mut iter)
        }
    };

    Ok(quote! { { #maybe_debug_line #tokens } })
}

fn expand_tuple_struct(structure: PackTupleStruct, debug: bool) -> Result<TokenStream2, darling::Error> {
    let PackTupleStruct(types) = structure;

    let maybe_debug_line = if debug {
        let str = format!("{:?}", types);
        quote! { println!("expand_tuple_struct({})", #str); }
    } else { quote! { } };

    // Deplorable hacky workaround for primitives having no children
    if types.len() == 1 {
        match types[0] {
            PackerType::NaiveTime =>
                return Ok(quote! {
                    lang_packer_model::pack_trees::get_naive_time_from_tree(tree)
                        .map(Self)
                }),
            PackerType::Usize =>
                return Ok(quote! {
                    lang_packer_model::pack_trees::get_usize_from_tree(tree)
                        .map(Self)
                }),
            PackerType::String =>
                return Ok(quote! {
                    lang_packer_model::pack_trees::get_string_from_tree(tree)
                        .map(Self)
                }),
            PackerType::Option =>
                return Ok(quote! {
                    lang_packer_model::pack_trees::unpack_maybe_one_tree(tree)
                        .map(Self)
                }),
            PackerType::Vec | PackerType::Box | PackerType::UserType(_) =>
                ()
        }
    }

    let tree_idents = types.iter()
        .enumerate()
        .map(|(n, _)| format_ident!("value_{}", n))
        .collect::<Vec<_>>();

    let packers = types.into_iter()
        .map(|p| get_packer_snippet(p, debug))
        .collect::<Result<Vec<_>, _>>()?;

    let output = quote! {
        let mut iter = lang_packer_model::pack_trees::get_tree_children(tree, <Self as lang_packer_model::pack_trees::HasRule>::get_rule())?
            .into_iter()
            .peekable();

        #(let #tree_idents = #packers?;)*

        lang_packer_model::pack_trees::ensure_no_more_trees(iter)?;

        Ok(Self(#(#tree_idents),*))
    };

    Ok(quote!{ { #maybe_debug_line #output } })
}

fn expand_data_structure(input: DarlingRuleAttr, debug: bool /* TODO this isn't good practice */) -> Result<TokenStream2, darling::Error> {
    let struct_ident = input.ident;

    let (rule_type, rule_variant) = input.rule.segments.into_iter()
        .rev().take(2).rev() // Take from end
        .map(|p| p.ident)
        .collect_tuple()
        .expect("TODO error message take last two segments of path");

    let pack_body = match input.data.into() {
        PackDataStructure::Enum(ds) =>
            expand_enum(ds, debug),

        PackDataStructure::UnitStruct =>
            expand_unit_struct(debug),

        PackDataStructure::TupleStruct(ds) =>
            expand_tuple_struct(ds, debug),
    }?;

    let output = quote! {
        impl lang_packer_model::pack_trees::HasRule for #struct_ident {
            type Rule = #rule_type;

            fn get_rule() -> Self::Rule {
                #rule_type::#rule_variant
            }
        }

        impl lang_packer_model::pack_trees::TokenPacker for #struct_ident {
            fn pack_impl(
                tree: lang_packer_model::generic_utils::SyntaxTree<Self::Rule>
            ) -> Result<Self, lang_packer_model::generic_utils::PackingError<Self::Rule>> {
                #pack_body
            }
        }
    };

    Ok(output)
}

#[proc_macro_derive(Packer, attributes(packer))]
pub fn derive_packer(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    FromDeriveInput::from_derive_input(&input)
        .and_then(|attr| expand_data_structure(attr, false))
        .map(Into::into)
        .unwrap_or_else(|e| e.write_errors().into())
}

#[proc_macro_derive(DbgPacker, attributes(packer))]
pub fn derive_dbg_packer(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    FromDeriveInput::from_derive_input(&input)
        .and_then(|attr| expand_data_structure(attr, true))
        .map(Into::into)
        .unwrap_or_else(|e| e.write_errors().into())
}
