use crate::errors::ShellError;
use crate::parser::{
    hir,
    hir::syntax_shape::{
        continue_expression, expand_expr, maybe_spaced, spaced, AnyExpressionShape, BareShape,
        ExpandContext, ExpandExpression, ExpandSyntax, PathTailShape,
    },
    hir::{debug_tokens, TokensIterator},
    RawToken, TokenNode,
};
use crate::{Tag, Tagged, TaggedItem};

#[derive(Debug, Copy, Clone)]
pub struct ExpressionListShape;

impl ExpandSyntax for ExpressionListShape {
    type Output = Vec<hir::Expression>;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<Vec<hir::Expression>, ShellError> {
        let mut exprs = vec![];

        if token_nodes.at_end_possible_ws() {
            return Ok(exprs);
        }

        let expr = expand_expr(&maybe_spaced(AnyExpressionShape), token_nodes, context)?;

        exprs.push(expr);

        println!("{:?}", debug_tokens(token_nodes, context.source));

        loop {
            if token_nodes.at_end_possible_ws() {
                return Ok(exprs);
            }

            let expr = expand_expr(&spaced(AnyExpressionShape), token_nodes, context)?;

            exprs.push(expr);
        }
    }
}