use rustc_hash::FxHashMap;

use crate::{ArgDecl, Atom, Literal, Rule, Program, Predicate, PredicateKind};
use syn::parse::{Parse, ParseStream};
use syn::punctuated;
use syn::{Token, parenthesized, punctuated::Punctuated};
use proc_macro2::{TokenStream, Ident};

mod kw {
    syn::custom_keyword!(relation);
    syn::custom_keyword!(irelation);
}

impl Parse for PredicateKind {

    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::relation) {
            input.parse::<kw::relation>()?;
            Ok(PredicateKind::Extensional)
        } else if lookahead.peek(kw::irelation) {
            input.parse::<kw::irelation>()?;
            Ok(PredicateKind::Intensional)
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for ArgDecl {

    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let rust_type = input.parse()?;
        Ok(ArgDecl {
            name,
            rust_type,
        })
    }
}

impl Parse for Predicate {

    fn parse(input: ParseStream) -> syn::Result<Self> {
        let kind = input.parse()?;
        let name = input.parse()?;
        let content;
        parenthesized!(content in input);
        let parsed_content: Punctuated<ArgDecl, Token![,]> = content.parse_terminated(ArgDecl::parse)?;
        let parameters = parsed_content.into_pairs().map(|pair| pair.into_value()).collect();
        Ok(Predicate { kind, name, parameters })
    }
}

impl Parse for Atom {

    fn parse(input: ParseStream) -> syn::Result<Self> {
        let predicate = input.parse()?;
        let content;
        parenthesized!(content in input);
        let punctuated: Punctuated<Ident, Token![,]> = content.parse_terminated(Ident::parse)?;
        let args = punctuated.into_pairs().map(|pair| pair.into_value()).collect();
        Ok(Atom { predicate, args })
    }
}

impl Parse for Rule {

    fn parse(input: ParseStream) -> syn::Result<Self> {
        let head = input.parse()?;
        // TODO: Dissallow space between : and -
        input.parse::<Token![:]>()?;
        input.parse::<Token![-]>()?;
        let body: Punctuated<Ident, Token![,]> = Punctuated::parse_separated_nonempty(input)?;
        Ok(Rule {
            head, body: Vec::new()
        })
    }
}

impl Parse for Program {

    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut predicates = Vec::new();
        let mut rules = Vec::new();
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(kw::relation) || lookahead.peek(kw::irelation) {
                let predicate: Predicate = input.parse()?;
                predicates.push(predicate);
            } else {
                let rule: Rule = input.parse()?;
                rules.push(rule);
            }
        }
        Ok(Program {
            predicates: predicates,
            rules: rules,
        })
    }
}

/// Primitive and inefficient parser for _valid_ datalog, with no error checking. Basically deserializing
/// a list of `Display` representations of a `Rule`, with a couple tweaks to make it easier
/// to write and use:
/// - Ignores empty lines and the ones starting with `//`comments
/// - ignores whitespace between tokens
pub fn parse(text: &str) -> Program {
    eprintln!("text: {}", text);
    match syn::parse_str(text) {
        Ok(program) => program,
        Err(err) => panic!("Error: {:?}", err),
    }

//  let mut rules = Vec::new();

//  for rule in text.split(".").map(|s| s.trim()).filter(|s| !s.is_empty()) {
//      let parts: Vec<_> = rule.split(":-").map(|s| s.trim()).collect();
//      let head = parts[0];
//      let body = parts[1].split("),");

//      let head = {
//          let idx = head.find("(").unwrap();
//          let predicate = &head[..idx];
//          let args = &head[idx..];

//          let start = args.find("(").unwrap() + 1;
//          let end = args.find(")").unwrap();
//          let args: Vec<_> = args[start..end].split(", ").collect();

//          Atom::new(predicate, args)
//      };

//      let body = {
//          let string_literals = body.map(|s| s.trim());
//          let mut body = Vec::new();

//          for literal in string_literals {
//              let idx = literal.find("(").unwrap();
//              let mut predicate = &literal[..idx];
//              let mut args = &literal[idx..];

//              let is_negated = {
//                  if predicate.starts_with("!") {
//                      predicate = &predicate[1..];
//                      true
//                  } else {
//                      false
//                  }
//              };

//              let start = args.find("(").unwrap() + 1;
//              if let Some(end) = args.find(")") {
//                  args = &args[start..end];
//              } else {
//                  args = &args[start..];
//              }

//              let args: Vec<_> = args.split(", ").collect();

//              let literal = if is_negated {
//                  Literal::new_anti(predicate, args)
//              } else {
//                  Literal::new(predicate, args)
//              };

//              body.push(literal);
//          }

//          body
//      };

//      let rule = Rule { head, body };
//      rules.push(rule);
//  }

//  rules
}

// Primitive parser of relation declarations:
// - one-per line
// - the syntax is similar to Soufflé's decls:
//   `.decl $relation($arg_a: TypeA, $arg_b: TypeB, ...)`
//
// The goal is to map from a relation name to its canonical argument names and ordering,
// which you can't have solely in rules (where variables/arguments can have arbitrary names).
// This is used when generating skeleton datafrog computations, to help naming the
// relation indices by the canonical variable names used in the index key.
pub fn parse_declarations(decls: &str) -> FxHashMap<String, Vec<ArgDecl>> {
    unimplemented!();
    /*
    let mut declarations = FxHashMap::default();
    for line in decls.lines().map(|l| l.trim()).filter(|l| !l.is_empty()) {
        let prefix = ".decl ".len();
        let line = &line[prefix..];

        let idx = line.find("(").unwrap();
        let predicate = &line[..idx];
        let args = &line[idx..];

        let start = args.find("(").unwrap() + 1;
        let end = args.find(")").unwrap();
        let args: Vec<_> = args[start..end]
            .split(",")
            .map(|arg| {
                let mut typed_arg_decl = arg.trim().split(":");
                let name = typed_arg_decl.next().unwrap().to_lowercase();
                let rust_type = typed_arg_decl.next().unwrap().trim().to_string();

                ArgDecl { name, rust_type }
            })
            .collect();

        declarations.insert(predicate.to_string(), args);
    }
    declarations
    */
}

pub fn clean_program(text: String) -> String {
    text.lines()
        .map(|s| s.trim())
        .filter(|line| !line.starts_with("//"))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_valid_datalog() {
        let program = parse("relation P(x: u32, y: u64)");
        let program = parse("p(x, y) :- e(x, y). p(x, z) :- e(x, y), p(y, z).");
        //assert_eq!("p(x, y) :- e(x, y).", program.rules[0].to_string());
        //assert_eq!("p(x, z) :- e(x, y), p(y, z).", program.rules[1].to_string());
    }

    #[test]
    fn parse_multiline_datalog() {
        let text = r#"
            subset(O1, O2, P)    :- outlives(O1, O2, P).
            subset(O1, O3, P)    :- subset(O1, O2, P), subset(O2, O3, P).
            subset(O1, O2, Q)    :- subset(O1, O2, P), cfg_edge(P, Q), region_live_at(O1, Q), region_live_at(O2, Q).
            requires(O, L, P)    :- borrow_region(O, L, P).
            requires(O2, L, P)   :- requires(O1, L, P), subset(O1, O2, P).
            requires(O, L, Q)    :- requires(O, L, P), !killed(L, P), cfg_edge(P, Q), region_live_at(O, Q).
            borrow_live_at(L, P) :- requires(O, L, P), region_live_at(O, P).
            errors(L, P)         :- invalidates(L, P), borrow_live_at(L, P)."#;

        let program = parse(text);
        let serialized = program.rules
            .into_iter()
            .map(|rule| rule.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        let expected = r#"subset(O1, O2, P) :- outlives(O1, O2, P).
subset(O1, O3, P) :- subset(O1, O2, P), subset(O2, O3, P).
subset(O1, O2, Q) :- subset(O1, O2, P), cfg_edge(P, Q), region_live_at(O1, Q), region_live_at(O2, Q).
requires(O, L, P) :- borrow_region(O, L, P).
requires(O2, L, P) :- requires(O1, L, P), subset(O1, O2, P).
requires(O, L, Q) :- requires(O, L, P), !killed(L, P), cfg_edge(P, Q), region_live_at(O, Q).
borrow_live_at(L, P) :- requires(O, L, P), region_live_at(O, P).
errors(L, P) :- invalidates(L, P), borrow_live_at(L, P)."#;
        assert_eq!(expected, serialized);
    }

    #[test]
    fn parse_multiline_datalog_with_comments() {
        let text = r#"
            // `subset` rules
            subset(O1, O2, P) :- outlives(O1, O2, P).

            subset(O1, O3, P) :- subset(O1, O2, P),
                                   subset(O2, O3, P).
            subset(O1, O2, Q) :-
              subset(O1, O2, P),
              cfg_edge(P, Q),
              region_live_at(O1, Q),
              region_live_at(O2, Q).

            // `requires` rules
            requires(O, L, P) :- borrow_region(O, L, P).

            requires(O2, L, P) :-
              requires(O1, L, P),subset(O1, O2, P).

            requires(O, L, Q) :-
              requires(O, L, P),
                       !killed(L, P),    cfg_edge(P, Q),
    region_live_at(O, Q).

            // this one is commented out, nope(N, O, P, E) :- open(O, P, E, N).

            borrow_live_at(L, P) :-
              requires(O, L, P),
              region_live_at(O, P).

            errors(L, P) :-
              invalidates(L, P),
              borrow_live_at(L, P)."#;

        let program = clean_program(text.to_string());
        let rules = parse(&program).rules;

        let serialized = rules
            .into_iter()
            .map(|rule| rule.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        let expected = r#"subset(O1, O2, P) :- outlives(O1, O2, P).
subset(O1, O3, P) :- subset(O1, O2, P), subset(O2, O3, P).
subset(O1, O2, Q) :- subset(O1, O2, P), cfg_edge(P, Q), region_live_at(O1, Q), region_live_at(O2, Q).
requires(O, L, P) :- borrow_region(O, L, P).
requires(O2, L, P) :- requires(O1, L, P), subset(O1, O2, P).
requires(O, L, Q) :- requires(O, L, P), !killed(L, P), cfg_edge(P, Q), region_live_at(O, Q).
borrow_live_at(L, P) :- requires(O, L, P), region_live_at(O, P).
errors(L, P) :- invalidates(L, P), borrow_live_at(L, P)."#;
        assert_eq!(expected, serialized);
    }
}
