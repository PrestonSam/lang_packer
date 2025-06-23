///////////////////////////////
/// This file contains experimental content
///////////////////////////////


use std::{iter::Peekable, marker::PhantomData};

use chrono::NaiveTime;
use itertools::{Itertools, PeekingNext};
use pest::RuleType;

use crate::{generic_utils::{PackingError, PackingErrorVariant, RuleAndProvidence, SyntaxTree}, pack_trees::TokenPacker};

pub struct PackWrap<R: RuleType>(PhantomData<R>);

impl<R: RuleType> PackWrap<R> {
    // Internal
    fn get_tree_children_with_rule(tree: SyntaxTree<R>, expected_rule: R) -> Result<Option<Vec<SyntaxTree<R>>>, PackingError<R>> {
        match tree {
            SyntaxTree { rule, children, .. } if rule == expected_rule =>
                Ok(children),
            
            SyntaxTree { rule, providence, .. } =>
                Err(PackingError::new(PackingErrorVariant::WrongRule { found_rule: RuleAndProvidence(rule, providence.as_string()) }))
        }
    }

    // Internal
    fn expect_trees(trees: Option<Vec<SyntaxTree<R>>>) -> Result<Vec<SyntaxTree<R>>, PackingError<R>> {
        trees.ok_or_else(|| PackingError::new(PackingErrorVariant::NoChildrenFound))
    }

    // Used for entrypoints. TODO try make internal only
    pub fn unpack_only_tree(trees: Vec<SyntaxTree<R>>) -> Result<SyntaxTree<R>, PackingError<R>> {
        let mut iter = trees.into_iter();

        match (iter.next(), iter.next()) {
            (Some(tree), None) =>
                Ok(tree),

            _ =>
                Err(PackingError::new(PackingErrorVariant::ExpectedExactlyOneChild {
                    found_child_count: iter.len() + 2
                })),
        }
    }

    // Used for String
    pub fn get_tree_src_string(tree: SyntaxTree<R>, expected_rule: R) -> Result<String, PackingError<R>> {
        match tree {
            SyntaxTree { rule, providence, children: None } if rule == expected_rule =>
                Ok(providence.as_trimmed_string()),
            
            SyntaxTree { rule, providence, .. } =>
                Err(PackingError::new(PackingErrorVariant::WrongRule { found_rule: RuleAndProvidence(rule, providence.as_string()) }))
        }
    }

    pub fn get_string_from_tree(tree: SyntaxTree<R>) -> Result<String, PackingError<R>> {
        Ok(tree.as_string())
    }

    // Used for usize
    pub fn get_usize_from_tree(tree: SyntaxTree<R>) -> Result<usize, PackingError<R>> {
        tree.as_string()
            .parse()
            .map_err(|e| PackingErrorVariant::ParseUsizeError { inner: e, src: tree.as_string()})
            .map_err(PackingError::new) 
    }

    // Used for NaiveTime
    pub fn get_naive_time_from_tree(tree: SyntaxTree<R>) -> Result<NaiveTime, PackingError<R>> {
        let time_str = tree.as_string();

        NaiveTime::parse_from_str(&time_str, "%H:%M")
            .map_err(PackingErrorVariant::ParseTimeError)
            .map_err(PackingError::new) 
    }

    // Used for enums
    pub fn get_only_tree_child(tree: SyntaxTree<R>, expected_rule: R) -> Result<SyntaxTree<R>, PackingError<R>> {
        Self::get_tree_children_with_rule(tree, expected_rule)
            .and_then(Self::expect_trees)
            .and_then(Self::unpack_only_tree)
    }

    // Used for enums
    pub fn make_wrong_rules_alt_error(tree: SyntaxTree<R>, expected_rules: Vec<R>) -> PackingError<R> {
        let found_rule = RuleAndProvidence(tree.rule, tree.providence.as_string());
        let variant = PackingErrorVariant::WrongRulesAlt { found_rule, expected_rules };

        PackingError::new(variant)
    }

    // Used for (P, ..) as well as internal
    pub fn get_tree_children(tree: SyntaxTree<R>, expected_rule: R) -> Result<Vec<SyntaxTree<R>>, PackingError<R>> {
        Self::get_tree_children_with_rule(tree, expected_rule)
            .and_then(Self::expect_trees)
    }

    pub fn get_next_tree<'a, I>(trees: &mut I) -> Result<SyntaxTree<'a, R>, PackingError<R>>
    where
        I: Iterator<Item = SyntaxTree<'a, R>>,
    {
        trees.next()
            .ok_or(PackingErrorVariant::ExpectedChild)
            .map_err(PackingError::new)
    }

    pub fn get_vec_of_packer<'a, I, P>(trees: &mut I) -> Result<Vec<P>, PackingError<P::Rule>>
    where
        I: Iterator<Item = SyntaxTree<'a, P::Rule>>,
        P: TokenPacker,
    {
        trees.peekable()
            .peeking_take_while(P::is_packable)
            .map(P::pack)
            .collect()
    }

    pub fn maybe_pack_next_tree<'a, I, P>(trees: &mut Peekable<I>) -> Result<Option<P>, PackingError<P::Rule>>
    where
        I: Iterator<Item = SyntaxTree<'a, P::Rule>>,
        P: TokenPacker,
    {
        trees.peekable()
            .peeking_next(P::is_packable)
            .map(P::pack)
            .transpose()
    }

    // Used for (P, ..)
    pub fn pack_next_tree<'a, I, P>(trees: &mut I) -> Result<P, PackingError<P::Rule>>
    where
        I: Iterator<Item = SyntaxTree<'a, P::Rule>>,
        P: TokenPacker,
    {
        trees.next()
            .ok_or_else(|| PackingError::new(PackingErrorVariant::ExpectedChildWithRule { expected_rule: P::get_rule() }))
            .and_then(P::pack)
    }

    // Used for (P, ..)
    pub fn ensure_no_more_trees<'a, I>(mut trees: I) -> Result<(), PackingError<R>>
    where
        I: Iterator<Item = SyntaxTree<'a, R>>
    {
        match trees.next() {
            None =>
                Ok(()),
            Some(fst) => {
                let rules = std::iter::once(fst)
                    .chain(trees)
                    .map(|t| t.rule)
                    .collect_vec();

                Err(PackingError::new(PackingErrorVariant::TooManyChildren {
                    found_child_count: rules.len(),
                    found_rules: rules
                }))
            },
        }
    }

    // Used for Vec<P>
    pub fn unpack_tree_pack_each_child<P>(tree: SyntaxTree<P::Rule>, expected_rule: P::Rule) -> Result<Vec<P>, PackingError<P::Rule>>
    where P : TokenPacker<Rule = R>
    {
        Self::get_tree_children(tree, expected_rule)?
            .into_iter()
            .map(P::pack)
            .collect::<Result<_, _>>()
    }

    // Used for Option<P>
    pub fn unpack_maybe_one_tree<P>(tree: SyntaxTree<P::Rule>) -> Result<Option<P>, PackingError<P::Rule>>
    where
        P: TokenPacker<Rule = R>
    {
        Self::get_tree_children_with_rule(tree, P::get_rule())
            .and_then(|maybe_children|
                match maybe_children {
                    Some(children) =>
                        Self::unpack_only_tree(children)
                            .and_then(P::pack)
                            .map(Some),

                    None =>
                        Ok(None),
                })
    }
}