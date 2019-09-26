use crate::cli::external_command;
use crate::commands::{classified::InternalCommand, ClassifiedCommand, Command};
use crate::parser::hir::syntax_shape::{
    block::AnyBlockShape, expand_variable, parse_single_node, ExpandContext, ExpandExpression,
    TestSyntax,
};
use crate::parser::hir::tokens_iterator::Peeked;
use crate::parser::parse_command::parse_command_tail;
use crate::parser::{hir, hir::TokensIterator, Operator, RawToken, Token, TokenNode};
use crate::prelude::*;
use derive_new::new;
use getset::Getters;
use log::trace;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug, Copy, Clone)]
pub struct NumberShape;

impl ExpandExpression for NumberShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(token_nodes, "Number", |token, token_tag| {
            Ok(match token {
                RawToken::GlobPattern => {
                    return Err(ShellError::type_error(
                        "Number",
                        "glob pattern".to_string().tagged(token_tag),
                    ))
                }
                RawToken::Operator(..) => {
                    return Err(ShellError::type_error(
                        "Number",
                        "operator".to_string().tagged(token_tag),
                    ))
                }
                RawToken::Variable(tag) if tag.slice(context.source) == "it" => {
                    hir::Expression::it_variable(tag, token_tag)
                }
                RawToken::ExternalCommand(tag) => hir::Expression::external_command(tag, token_tag),
                RawToken::ExternalWord => return Err(ShellError::invalid_external_word(token_tag)),
                RawToken::Variable(tag) => hir::Expression::variable(tag, token_tag),
                RawToken::Number(number) => {
                    hir::Expression::number(number.to_number(context.source), token_tag)
                }
                RawToken::Size(number, unit) => {
                    hir::Expression::size(number.to_number(context.source), unit, token_tag)
                }
                RawToken::Bare => hir::Expression::bare(token_tag),
                RawToken::String(tag) => hir::Expression::string(tag, token_tag),
            })
        })
    }
}
