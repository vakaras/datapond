//! This file contains the parse AST.

use proc_macro2::Ident;
use quote::ToTokens;
use std::fmt;

/// The relation kind regarding IO.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub(crate) enum RelationKind {
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
pub(crate) struct ParamDecl {
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

/// A declaration of the relation.
///
/// ```plain
/// irelation Input(x: u32, y: u32)
/// relation Internal(x: u32, y: u32)
/// orelation Output(x: u32, y: u32)
/// ```
#[derive(Debug, Clone)]
pub(crate) struct RelationDecl {
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

/// A positional argument `arg2`.
#[derive(Debug, Clone)]
pub(crate) enum PositionalArg {
    Ident(Ident),
    Wildcard,
}

impl fmt::Display for PositionalArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PositionalArg::Ident(ident) => write!(f, "{}", ident),
            PositionalArg::Wildcard => write!(f, "_"),
        }
    }
}

/// A named argument `.param2=arg2`.
#[derive(Debug, Clone)]
pub(crate) struct NamedArg {
    pub param: Ident,
    pub arg: Ident,
}

/// The list of atom's arguments.
#[derive(Debug, Clone)]
pub(crate) enum ArgList {
    /// arg1, arg2
    Positional(Vec<PositionalArg>),
    /// .param1=arg1, .param2=arg2
    Named(Vec<NamedArg>),
}

impl fmt::Display for ArgList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        match self {
            ArgList::Positional(args) => {
                for arg in args {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
            }
            ArgList::Named(args) => {
                for kwarg in args {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, ".{}={}", kwarg.param, kwarg.arg)?;
                }
            }
        }
        Ok(())
    }
}

/// A richer type of relation/atom, which can be negated, and used as
/// premises/hypotheses in rules.
#[derive(Debug, Clone)]
pub(crate) struct Literal {
    pub is_negated: bool,
    pub relation: Ident,
    pub args: ArgList,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_negated {
            write!(f, "!")?;
        }
        write!(f, "{}({})", self.relation, self.args)
    }
}

/// A head of a rule.
#[derive(Debug, Clone)]
pub(crate) struct RuleHead {
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
#[derive(Debug, Clone)]
pub(crate) struct Rule {
    pub head: RuleHead,
    pub body: Vec<Literal>,
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} :- ", self.head)?;
        let mut first = true;
        for literal in &self.body {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", literal)?;
        }
        write!(f, ".")
    }
}

/// Items present in the program.
#[derive(Debug, Clone)]
pub(crate) enum ProgramItem {
    RelationDecl(RelationDecl),
    Rule(Rule),
}

impl fmt::Display for ProgramItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ProgramItem::RelationDecl(decl) => write!(f, "{}", decl),
            ProgramItem::Rule(rule) => write!(f, "{}", rule),
        }
    }
}

/// A Datalog program.
#[derive(Debug, Clone)]
pub(crate) struct Program {
    pub items: Vec<ProgramItem>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for item in &self.items {
            writeln!(f, "{}", item)?;
        }
        Ok(())
    }
}
