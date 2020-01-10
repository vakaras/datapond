use crate::generator_new::ast::*;
use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;

fn type_vec_to_tokens(type_vec: &Vec<syn::Type>) -> TokenStream {
    let mut type_tokens = TokenStream::new();
    for typ in type_vec {
        type_tokens.extend(quote! {#typ,});
    }
    type_tokens
}

fn var_vec_to_tokens(var_vec: &Vec<DVar>) -> TokenStream {
    let mut var_tokens = TokenStream::new();
    for var in var_vec {
        var_tokens.extend(quote! {#var,});
    }
    var_tokens
}

impl ToTokens for DVar {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        tokens.extend(quote! {#name});
    }
}

impl ToTokens for DVarTuple {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut vars = var_vec_to_tokens(&self.vars);
        tokens.extend(quote! {(#vars)});
    }
}

impl ToTokens for DVarKeyVal {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut key = var_vec_to_tokens(&self.key);
        let mut value = var_vec_to_tokens(&self.value);
        tokens.extend(quote! {((#key), (value))});
    }
}

impl ToTokens for DVars {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            DVars::Tuple(tuple) => tuple.to_tokens(tokens),
            DVars::KeyVal(key_val) => key_val.to_tokens(tokens),
        }
    }
}

impl ToTokens for DVarTypes {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            DVarTypes::Tuple(types) => {
                let type_tokens = type_vec_to_tokens(types);
                tokens.extend(quote! {(#type_tokens)});
            }
            DVarTypes::KeyVal { key, value } => {
                let key_tokens = type_vec_to_tokens(key);
                let value_tokens = type_vec_to_tokens(value);
                tokens.extend(quote! {((#key_tokens), (#value_tokens))});
            }
        }
    }
}

impl ToTokens for Variable {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        tokens.extend(quote! {#name});
    }
}

impl ToTokens for ReorderOp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ReorderOp {
            output,
            input,
            input_vars,
            output_vars,
        } = self;
        tokens.extend(quote! {
            #output.from_map(&#input, |&#input_vars| #output_vars);
        });
    }
}

impl ToTokens for BindVarOp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        unimplemented!();
    }
}

impl ToTokens for JoinOp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        unimplemented!();
    }
}

impl ToTokens for FilterOp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        unimplemented!();
    }
}

impl ToTokens for Operation {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Operation::Reorder(op) => op.to_tokens(tokens),
            Operation::BindVar(op) => op.to_tokens(tokens),
            Operation::Join(op) => op.to_tokens(tokens),
            Operation::Filter(op) => op.to_tokens(tokens),
        }
    }
}

impl ToTokens for Iteration {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut declare_relations = TokenStream::new();
        for relation in self.relations.values() {
            let vec_name = &relation.var.name;
            let var = relation.var.to_token_stream();
            let mut typ = TokenStream::new();
            for var_typ in &relation.typ {
                typ.extend(quote! {#var_typ,});
            }
            declare_relations.extend(quote! {
                let #var = datafrog::Relation::from_vec::<(#typ)>(#vec_name);
            });
        }
        let mut declare_variables = TokenStream::new();
        let mut output_results = TokenStream::new();
        for variable in self.variables.values() {
            let var = variable.var.to_token_stream();
            let var_name = var.to_string();
            let typ = variable.typ.to_token_stream();
            declare_variables.extend(quote! {
                let #var = iteration.variable::<#typ>(#var_name);
            });
            if variable.is_output {
                output_results.extend(quote! {
                    let #var = #var.complete();
                });
            }
        }
        let mut operations = TokenStream::new();
        for operation in &self.operations {
            operation.to_tokens(&mut operations);
        }
        tokens.extend(quote! {
            let mut iteration = datafrog::Iteration::new();
            #declare_relations
            #declare_variables
            while iteration.changed() {
                #operations
            }
            #output_results
        });
    }
}
