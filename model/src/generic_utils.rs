use thiserror::Error;

use std::{fmt::{Debug, Display}, num::ParseIntError, ops::Not};
use itertools::Itertools;
use pest::{iterators::Pair, RuleType, Span};

#[derive(Clone)]
pub struct Providence<'a> {
    pub span: Span<'a>,
    pub src: &'a str,
}

impl<'a> Providence<'a> {
    pub fn as_string(&self) -> String {
        self.src.to_string()
    }
    
     pub fn as_trimmed_string(&self) -> String {
        self.src.trim().to_string()
     }
}

fn trunc(str: &str, len: usize) -> String {
    if str.len() <= len {
        format!("{:?}", str)
    } else {
        format!("{:?}..", format!("{:.*}", len, str))
    }
}

impl<'a> std::fmt::Debug for Providence<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (line, column) = self.span.start_pos().line_col();

        write!(f, "At {}:{}, source code: {}", line, column, trunc(self.src, 40))
    }
}


// TODO might want to simplify this as providence & rules variants have been made redundant
#[derive(Debug)]
enum PackingErrorContext<R: RuleType> {
    Rule(R),
    Src(String),
}

impl<R: RuleType> Display for PackingErrorContext<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rule(rule) => f.write_fmt(format_args!("While parsing rule '{rule:?}'")),
            Self::Src(src) => f.write_fmt(format_args!("With source '{src}'")),
        }
    }
}

#[derive(Error, Debug) ]
pub struct PackingError<R>
where
    R: RuleType
{
    error: PackingErrorVariant<R>,
    context: Vec<PackingErrorContext<R>>,
    sibling_tokens: SiblingTokens,
}

impl<R> PackingError<R>
where
    R: RuleType
{
    pub fn new(error: PackingErrorVariant<R>) -> Self {
        PackingError {
            error,
            context: vec![],
            sibling_tokens: SiblingTokens::Unset,
        }
    }

    pub fn with_rule(mut self, rule: R) -> Self {
        self.context.push(PackingErrorContext::Rule(rule));
        self
    }

    pub fn with_providence(mut self, providence: &Providence<'_>) -> Self {
        self.context.push(PackingErrorContext::Src(providence.src.to_string()));
        self
    }

    pub fn with_sibling_tokens(mut self, siblings: String) -> Self {
        self.sibling_tokens = match self.sibling_tokens {
            SiblingTokens::Unset => SiblingTokens::Child,
            SiblingTokens::Child => SiblingTokens::Parent(siblings),
            p @ SiblingTokens::Parent(_) => p,
        };
        self
    }
}

impl<R> Display for PackingError<R>
where
    R: RuleType
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Packing error:\n")?;

        let mut rules = self.context.iter()
            .rev()
            .filter_map(|c| match c {
                PackingErrorContext::Rule(r) => Some(format!("{r:?}")),
                PackingErrorContext::Src(_) => None,
            });
        let mut providences = self.context.iter().filter_map(|c| match c {
            PackingErrorContext::Src(src) => Some(src.to_owned()),
            PackingErrorContext::Rule(_) => None,
        });

        f.write_fmt(format_args!("  At: {}\n", rules.join(" > ")))?;
        f.write_fmt(format_args!("  Error: {}\n", self.error))?;

        f.write_fmt(format_args!("  Siblings: {}\n", self.sibling_tokens))?;

        if let Some(self_providence) = providences.next() {
            f.write_fmt(format_args!("<Error source>\n{self_providence}\n</Error source>\n"))?;

            if let Some(parent_providence) = providences.next() {
                f.write_fmt(format_args!("<Parent source>\n{parent_providence}\n</Parent source>\n"))?;
            }
        }


        Ok(())
    }
}

pub trait DropRules: RuleType {
    fn get_drop_rules(&self) -> Vec<Self>;
}


#[derive(Clone)]
pub struct SyntaxTree<'a, R: RuleType> {
    pub rule: R,
    pub providence: Providence<'a>,
    pub children: Option<Vec<SyntaxTree<'a, R>>>,
}

impl<R: RuleType> SyntaxTree<'_, R> {
    pub fn dbg_direct_descendents(&self) -> String {
        match &self.children {
            Some(siblings) =>
                siblings.iter()
                    .map(|c| format!("{:?}", c.rule))
                    .join(", "),
            None => "<No tokens>".into(),
        }
    }
}

impl<'a, R: RuleType> SyntaxTree<'a, R> {
    pub fn as_string(&self) -> String {
        self.providence.as_string()
    }
}

impl<'a, R> From<Pair<'a, R>> for SyntaxTree<'a, R>
where R: RuleType + DropRules
{
    fn from(pair: Pair<'a, R>) -> Self {
        let rule = pair.as_rule();
        let providence = Providence { src: pair.as_str(), span: pair.as_span() };
        let skip_rules = rule.get_drop_rules();

        let children_vec = pair.into_inner()
            .filter(|pair| !(skip_rules.contains(&pair.as_rule())))
            .map(SyntaxTree::from)
            .collect_vec();

        let children = children_vec.is_empty().not()
            .then_some(children_vec);

        SyntaxTree { rule, providence, children }
    }
}

impl<'a, R: RuleType> Debug for SyntaxTree<'a, R>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxTree { rule, providence, children: None } =>
                f.debug_struct("TreeLeaf")
                    .field("rule", rule)
                    .field("providence", providence)
                    .finish(),

            SyntaxTree { rule, providence, children: Some(children) } =>
                f.debug_struct("TreeNode")
                    .field("rule", rule)
                    .field("providence", providence)
                    .field("children", children)
                    .finish(),
        }
    }
}


#[derive(Debug)]
pub struct RuleAndProvidence<R: RuleType>(pub R, pub String);

const MAX_LENGTH: usize = 32;

impl<R: RuleType> Display for RuleAndProvidence<R> {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self.0))?;

        if self.1.chars().count() < MAX_LENGTH {
            f.write_str(&self.1)
        } else {
            let providence = self.1.chars()
                .take(MAX_LENGTH)
                .chain("...".chars())
                .collect::<String>();

            f.write_fmt(format_args!("({:?})", providence))
        }
    }
}

#[derive(Debug)]
pub enum SiblingTokens {
    Unset,
    Child,
    Parent(String),
}

impl std::fmt::Display for SiblingTokens {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            SiblingTokens::Unset => "<PestPacker Error! No siblings set>",
            SiblingTokens::Child => "<No siblings>",
            SiblingTokens::Parent(str) => str,
        };

        f.write_str(str)
    }
}

#[derive(Debug)]
pub enum PackingErrorVariant<R: RuleType> {
    WrongRulesAlt { found_rule: RuleAndProvidence<R>, expected_rules: Vec<R> },
    WrongRule { found_rule: RuleAndProvidence<R> },
    ExpectedChildWithRule { expected_rule: R },
    ExpectedChild,
    ExpectedExactlyOneChild { found_child_count: usize },
    NoChildrenFound,
    TooManyChildren { found_child_count: usize, found_rules: Vec<R> },
    ParseUsizeError { inner: ParseIntError, src: String },
    ParseTimeError(chrono::ParseError),
}

impl<R: RuleType> PackingErrorVariant<R> {
    fn debug_vec<T>(key: &str, maybe_values: Option<&Vec<T>>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    where T: std::fmt::Debug
    {
        match maybe_values {
            Some(values) =>
                f.debug_set() // TODO double check this works instead of the old method
                    .entries(values)
                    .finish(),
                // Old method below
                // f.write_fmt(format_args!("  {key}:\n    {}\n", values.iter().map(|v| format!("{v:?}")).join(", "))),

            None =>
                f.write_fmt(format_args!("  {key}:\n    [No rules]\n")),
        }
    }
}

impl<R: RuleType> Display for PackingErrorVariant<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::WrongRulesAlt { found_rule, expected_rules } => {
                f.write_str("Encountered unexpected rule:\n")?;
                Self::debug_vec("Expected any one of the following rules", Some(expected_rules), f)?;
                f.write_fmt(format_args!("  Instead found the following rule {found_rule}"))?;

                Ok(())
            }
            Self::ExpectedChildWithRule { expected_rule } =>
                f.write_fmt(format_args!("Expected child with rule {expected_rule:?} but found none.")),

            Self::ParseUsizeError { inner, src } =>
                f.write_fmt(format_args!("{inner} | Found {src}")),

            Self::ParseTimeError(chrono_parse_err) =>
                f.write_fmt(format_args!("{}", chrono_parse_err)),

            PackingErrorVariant::WrongRule { found_rule } => {
                f.write_str("Encountered wrong rule\n")?;
                f.write_fmt(format_args!("  Found {found_rule}"))
            },

            PackingErrorVariant::ExpectedExactlyOneChild { found_child_count } => {
                f.write_str("Expected exactly one child")?;
                f.write_fmt(format_args!("  Found {found_child_count} children\n"))
            },

            PackingErrorVariant::ExpectedChild =>
                f.write_fmt(format_args!("  Expected a child, but found none")),

            PackingErrorVariant::NoChildrenFound =>
                f.write_str("Expected children but found none"),
            
            // TODO this is not very informative. How many children were expected?
            PackingErrorVariant::TooManyChildren { found_child_count, found_rules } => {
                f.write_str("Expected fewer children")?;
                f.write_fmt(format_args!("  Found {found_child_count} spare children: {found_rules:?}\n"))
            }
        }
    }
}


pub trait PackingResult<R: RuleType> {
    fn with_rule(self, rule: R) -> Self;

    fn with_providence(self, providence: &Providence<'_>) -> Self;

    fn with_sibling_tokens(self, siblings: String) -> Self;
}

impl<T, R: RuleType> PackingResult<R> for Result<T, PackingError<R>> {
    fn with_rule(self, rule: R) -> Self {
        self.map_err(|err| err.with_rule(rule))
    }

    fn with_providence(self, providence: &Providence<'_>) -> Self {
        self.map_err(|err| err.with_providence(providence))
    }

    fn with_sibling_tokens(self, siblings: String) -> Self {
        self.map_err(|err| err.with_sibling_tokens(siblings))
    }
}
