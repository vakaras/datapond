//! A module with helpers to work with datalog and datafrog: containing
//! - a simple data model to analyze, and compute rule transformations.
//! - a primitive parser for _valid_ syntax creating instances of the data model.
//! - a simple datalog-to-datafrog generator which will generate a skeleton
//! datafrog computation of the datalog rules, including preparing data in
//! `Relations`, the computed `Variables`, the join/antijoin/map operations
//! translations of the rules, and setup and maintenance of the indices used during
//! the joins and their possibly intermediate steps.

use proc_macro2::Ident;
use quote::ToTokens;
use std::fmt;
use std::ops::Deref;

/// Whether a predicate is used only as input, or produces new tuples.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum PredicateKind {
    Extensional,
    Intensional,
}

/// Records the argument information of relation declarations
#[derive(Clone)]
pub struct ArgDecl {
    pub name: Ident,
    pub rust_type: syn::Type,
}

impl fmt::Debug for ArgDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.rust_type.to_token_stream())
    }
}

/// Predicate declaration.
#[derive(Debug, Clone)]
pub struct Predicate {
    pub kind: PredicateKind,
    pub name: Ident,
    pub parameters: Vec<ArgDecl>,
}

/// An atom, or relational atom, is a building block used in rules, also known as subgoal,
/// describing a relation name and the name of its components.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Atom {
    pub predicate: Ident,
    pub args: Vec<Ident>,
}

/// A richer type of relation/atom, which can be negated, and used as premises/hypotheses in rules.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Literal {
    pub atom: Atom,
    pub is_negated: bool,
    pub kind: PredicateKind,
}

/// A specific type of Horn clause relating the premises/hypotheses/antecedents/conditions in its body
/// to the conclusion/consequent in its head.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Rule {
    pub head: Atom,
    pub body: Vec<Literal>,
}

pub struct Program {
    pub predicates: Vec<Predicate>,
    pub rules: Vec<Rule>,
}

impl Atom {
    pub fn new(predicate: Ident, args: Vec<Ident>) -> Self {
        Atom {
            predicate: predicate,
            args,
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.predicate)?;
        for (idx, arg) in self.args.iter().enumerate() {
            write!(f, "{}", arg)?;
            if idx < self.args.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl Literal {
    pub fn new(predicate: Ident, args: Vec<Ident>) -> Self {
        Self {
            atom: Atom::new(predicate, args),
            is_negated: false,
            kind: PredicateKind::Extensional,
        }
    }

    pub fn new_anti(predicate: Ident, args: Vec<Ident>) -> Self {
        Self {
            atom: Atom::new(predicate, args),
            is_negated: true,
            kind: PredicateKind::Extensional,
        }
    }
}

impl Deref for Literal {
    type Target = Atom;

    fn deref(&self) -> &Self::Target {
        &self.atom
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_negated {
            write!(f, "!")?;
        }
        write!(f, "{}", self.atom)
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} :- ", self.head)?;
        for (idx, h) in self.body.iter().enumerate() {
            write!(f, "{}", h)?;
            if idx < self.body.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ".")
    }
}
