use std::fmt;
use std::ops::Deref;
use std::collections::HashMap;
use proc_macro2::Ident;
use quote::ToTokens;

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
pub enum Atom {
    Positional { predicate: Ident, args: Vec<Ident> },
    Named { predicate: Ident, args: Vec<(Ident, Ident)> },
}

impl Atom {
    pub fn predicate(&self) -> &Ident {
        match self {
            Atom::Positional { predicate, .. } => predicate,
            Atom::Named { predicate, .. } => predicate,
        }
    }
}

/// A richer type of relation/atom, which can be negated, and used as premises/hypotheses in rules.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Literal {
    pub atom: Atom,
    pub is_negated: bool,
}


/// A specific type of Horn clause relating the premises/hypotheses/antecedents/conditions in its body
/// to the conclusion/consequent in its head.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Rule {
    pub head: Atom,
    pub body: Vec<Literal>,
}

pub enum ProgramItem {
    Predicate(Predicate),
    Rule(Rule),
}

pub struct Program {
    pub items: Vec<ProgramItem>,
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Positional { predicate, args } => {
                write!(f, "{}(", predicate)?;
                for (idx, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if idx < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
            },
            Atom::Named { predicate, args } => {
                write!(f, "{}(", predicate)?;
                for (idx, (name, arg)) in args.iter().enumerate() {
                    write!(f, ".{} = {}", name, arg)?;
                    if idx < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
            }
        }
        write!(f, ")")
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

impl fmt::Display for Predicate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!();
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

impl fmt::Display for ProgramItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ProgramItem::Predicate(this) => this.fmt(f),
            ProgramItem::Rule(this) => this.fmt(f),
        }
    }
}
