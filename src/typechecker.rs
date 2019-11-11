use std::collections::HashMap;

use crate::datapond::{ArgDecl, Atom, Literal, Rule, Program, Predicate, PredicateKind, ProgramItem};

pub fn typecheck(program: Program) -> Result<(), (String, proc_macro2::Span)> {
    //
    let mut predicates: HashMap<String, &Predicate> = HashMap::new();

    for item in program.items.iter() {
        match item {
            ProgramItem::Predicate(predicate) => { predicates.insert(predicate.name.to_string(), predicate); },
            ProgramItem::Rule(Rule { head, body }) => {
                let predicate_name = head.predicate().to_string();
                if let Some(ref predicate) = predicates.get(&predicate_name) {
                    match head {
                        Atom::Positional { args, .. } => {
                            if args.len() != predicate.parameters.len() {
                                Err((format!("predicate {} expects {} parameters, {} found", predicate_name, predicate.parameters.len(), args.len()), head.predicate().span()))?;
                            }
                        },
                        Atom::Named { .. } => { Err(("a predicate name must not use named bindings".to_string(), head.predicate().span()))?; },
                    }
                } else {
                    Err((format!("predicate {} is not declared", predicate_name), head.predicate().span()))?;
                }
            },
        }
    }

    // TODO WIP WIP WIP

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typecheck_valid_datalog() {
        let text = r#"
            relation P(x: u32, y: u64)"#;
        let program = crate::parser::parse(text);
        assert!(typecheck(program).is_ok());
    }
}
