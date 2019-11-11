use rustc_hash::FxHashMap;

use crate::datapond::{ArgDecl, Atom, Literal, Rule, Program, Predicate, PredicateKind};
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

struct Kwarg {
    value: (Ident, Ident),
}

impl Parse for Kwarg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![.]>()?;
        let field: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let binding: Ident = input.parse()?;
        Ok(Kwarg { value: (field, binding) })
    }
}

impl Parse for Atom {

    fn parse(input: ParseStream) -> syn::Result<Self> {
        let predicate = input.parse()?;
        let content;
        parenthesized!(content in input);
        if content.peek(Token![.]) {
            let punctuated: Punctuated<Kwarg, Token![,]> = content.parse_terminated(Kwarg::parse)?;
            let args = punctuated.into_pairs().map(|pair| pair.into_value().value).collect();
            Ok(Atom::Named { predicate, args })
        } else {
            let punctuated: Punctuated<Ident, Token![,]> = content.parse_terminated(Ident::parse)?;
            let args = punctuated.into_pairs().map(|pair| pair.into_value()).collect();
            Ok(Atom::Positional { predicate, args })
        }
    }
}

impl Parse for Rule {

    fn parse(input: ParseStream) -> syn::Result<Self> {
        let head = input.parse()?;
        input.step(|cursor| {
            let rest = match cursor.token_tree() {
                Some((proc_macro2::TokenTree::Punct(punct), next)) if punct.as_char() == ':' && punct.spacing() == proc_macro2::Spacing::Joint => next,
                _ => { return Err(cursor.error(":- expected")) },
            };
            match rest.token_tree() {
                Some((proc_macro2::TokenTree::Punct(punct), next)) if punct.as_char() == '-' => Ok(((), next)),
                _ => Err(cursor.error(":- expected"))
            }
        });
        let body: Punctuated<Literal, Token![,]> = Punctuated::parse_separated_nonempty(input)?;
        input.parse::<Token![.]>()?;
        Ok(Rule { head, body: body.into_pairs().map(|pair| pair.into_value()).collect() })
    }
}

impl Parse for Literal {

    fn parse(input: ParseStream) -> syn::Result<Self> {
        let is_negated = input.peek(Token![!]);
        if is_negated {
            input.parse::<Token![!]>()?;
        }
        // eprintln!("NEXT {}", input.cursor().token_stream());
        let atom: Atom = input.parse()?;
        Ok(Literal {
            atom,
            is_negated,
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
                // eprintln!("parse {:?}", rule);
                rules.push(rule);
            }
        }
        Ok(Program {
            predicates: predicates,
            rules: rules,
        })
    }
}

/// TODO
pub fn parse(text: &str) -> Program {
    eprintln!("text: {}", text);
    match syn::parse_str(text) {
        Ok(program) => program,
        Err(err) => panic!("Error: {:?} (at {:?})", err, err.span().start()),
    }
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
    fn parse_named_args() {
        let program = parse("relation P(x: u32, y: u64)");
        let program = parse("p(x, y) :- e(.field1 = x, .field2 = y).");
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
