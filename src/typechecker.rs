use crate::ast;
use crate::parser::ast as past;
use proc_macro2::{Ident, Span};
use std::collections::{HashMap, HashSet};

use crate::datapond::{
    ArgDecl, Atom, Literal, Predicate, PredicateKind, Program, ProgramItem, Rule,
};

pub struct Error {
    pub msg: String,
    pub span: Span,
    pub hint_span: Option<Span>,
}

impl Error {
    fn new(msg: String, span: Span) -> Self {
        Self {
            msg: msg,
            span: span,
            hint_span: None,
        }
    }
    fn with_hint_span(msg: String, span: Span, hint_span: Span) -> Self {
        Self {
            msg: msg,
            span: span,
            hint_span: Some(hint_span),
        }
    }
}

fn check_head<'a>(
    head: &past::RuleHead,
    decls: &'a HashMap<String, ast::RelationDecl>,
) -> Result<&'a ast::RelationDecl, Error> {
    let decl = decls.get(&head.relation.to_string()).ok_or_else(|| {
        Error::new(
            format!("Unknown relation {}", head.relation),
            head.relation.span(),
        )
    })?;
    if head.args.len() != decl.parameters.len() {
        let msg = format!(
            "Wrong number of arguments: expected {}, found {}.",
            head.args.len(),
            decl.parameters.len()
        );
        return Err(Error::with_hint_span(
            msg,
            head.relation.span(),
            decl.name.span(),
        ));
    }
    Ok(decl)
}

fn check_body(
    body: Vec<past::Literal>,
    decls: &HashMap<String, ast::RelationDecl>,
) -> Result<Vec<ast::Literal>, Error> {
    let mut new_body = Vec::new();
    for literal in body {
        let decl = decls.get(&literal.relation.to_string()).ok_or_else(|| {
            Error::new(
                format!("Unknown relation {}", literal.relation),
                literal.relation.span(),
            )
        })?;
        let args = match literal.args {
            past::ArgList::Positional(positional_args) => {
                if positional_args.len() != decl.parameters.len() {
                    let msg = format!(
                        "Wrong number of arguments: expected {}, found {}.",
                        positional_args.len(),
                        decl.parameters.len()
                    );
                    return Err(Error::with_hint_span(
                        msg,
                        literal.relation.span(),
                        decl.name.span(),
                    ));
                }
                positional_args
                    .into_iter()
                    .map(|arg| match arg {
                        past::PositionalArg::Ident(ident) => ast::Arg::Ident(ident),
                        past::PositionalArg::Wildcard => ast::Arg::Wildcard,
                    })
                    .collect()
            }
            past::ArgList::Named(named_args) => {
                let kwargs: HashMap<_, _> = named_args
                    .into_iter()
                    .map(|named_arg| (named_arg.param.to_string(), named_arg.arg))
                    .collect();
                let mut args = Vec::new();
                let mut used_parameters = HashSet::new();
                for parameter in &decl.parameters {
                    let param_name = parameter.name.to_string();
                    let arg = match kwargs.get(&param_name) {
                        Some(ident) => {
                            let ident_str = ident.to_string();
                            if used_parameters.contains(&ident_str) {
                                return Err(Error::new(
                                    format!("Parameter already bound: {}", ident_str),
                                    ident.span(),
                                ));
                            }
                            used_parameters.insert(ident_str);
                            ast::Arg::Ident(ident.clone())
                        }
                        None => ast::Arg::Wildcard,
                    };
                    args.push(arg);
                }
                for key in kwargs.keys() {
                    if !used_parameters.contains(key) {
                        return Err(Error::new(
                            format!("Unknown parameter {} in relation.", key),
                            literal.relation.span(),
                        ));
                    }
                }
                if kwargs.len() != used_parameters.len() {}
                args
            }
        };
        let new_literal = ast::Literal {
            is_negated: literal.is_negated,
            relation: literal.relation,
            args: args,
        };
        new_body.push(new_literal);
    }
    Ok(new_body)
}

pub(crate) fn typecheck(program: past::Program) -> Result<ast::Program, Error> {
    let mut decls = HashMap::new();
    let mut rules = Vec::new();

    for item in program.items {
        match item {
            past::ProgramItem::RelationDecl(decl) => {
                decls.insert(decl.name.to_string(), decl);
            }
            past::ProgramItem::Rule(past::Rule { head, body }) => {
                let decl = check_head(&head, &decls);
                let body = check_body(body, &decls)?;
                rules.push(ast::Rule { head, body });
            }
        }
    }
    Ok(ast::Program {
        relation_decls: decls,
        rules: rules,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typecheck_valid_datalog() {
        let text = r#"
              relation P(x: u32, y: u64)
              irelation Q(x: u32, y: u64)

              P(x, y) :- Q(x, y).
              P(x, y) :- Q(.y=y, .x=x).

              "#;
        match typecheck(crate::parser::parse(text)) {
            Ok(program) => {
                assert_eq!(program.relation_decls.len(), 2);
                assert_eq!(program.rules.len(), 2);
                assert_eq!(program.rules[0], program.rules[1]);
            }
            Err(_) => unreachable!(),
        }
    }
}
