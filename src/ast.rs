//! This file contains the typed AST.

use proc_macro2::Ident;
use quote::ToTokens;
use std::collections::HashMap;
use std::fmt;

/// The relation kind regarding IO.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum RelationKind {
    /// Instances of this relation can be provided only as input facts.
    Input,
    /// Instances of this relation can be used for computation but cannot be output.
    Internal,
    /// Instances of this relation can be used for computation and also can be output.
    Output,
}

impl fmt::Display for RelationKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RelationKind::Input => write!(f, "irelation"),
            RelationKind::Internal => write!(f, "relation"),
            RelationKind::Output => write!(f, "orelation"),
        }
    }
}

/// Parameter information of the relation declaration.
#[derive(Clone)]
pub struct ParamDecl {
    pub name: Ident,
    pub typ: syn::Type,
}

impl fmt::Debug for ParamDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.typ.to_token_stream())
    }
}

impl fmt::Display for ParamDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.typ.to_token_stream())
    }
}

impl PartialEq for ParamDecl {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for ParamDecl {}

/// A declaration of the relation.
///
/// ```plain
/// irelation Input(x: u32, y: u32)
/// relation Internal(x: u32, y: u32)
/// orelation Output(x: u32, y: u32)
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelationDecl {
    pub kind: RelationKind,
    pub name: Ident,
    pub parameters: Vec<ParamDecl>,
}

impl fmt::Display for RelationDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}(", self.kind, self.name)?;
        let mut first = true;
        for parameter in &self.parameters {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", parameter)?;
        }
        write!(f, ")")
    }
}

/// An argument.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    /// Identifier `arg`.
    Ident(Ident),
    /// Wildcard argument.
    Wildcard,
}

/// A richer type of atom, which can be negated, and used as
/// premises/hypotheses in rules.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub is_negated: bool,
    pub relation: Ident,
    pub args: Vec<Arg>,
}

/// A head of a rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleHead {
    pub relation: Ident,
    pub args: Vec<Ident>,
}

impl fmt::Display for RuleHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.relation)?;
        let mut first = true;
        for arg in &self.args {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

/// A rule describing how to compute the relation facts.
///
/// ```plain
/// Internal(x, y) :- Input(x, y).
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rule {
    pub head: RuleHead,
    pub body: Vec<Literal>,
}

/// A Datalog program.
#[derive(Debug, Clone)]
pub struct Program {
    pub relation_decls: HashMap<String, RelationDecl>,
    pub rules: Vec<Rule>,
}
