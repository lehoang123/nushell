use crate::errors::ShellError;
use crate::parser::{
    hir,
    hir::{expand_expr, ExpandContext, ExpandExpression},
    TokensIterator,
};
use crate::Text;
use log::trace;

pub fn baseline_parse_tokens(
    token_nodes: &mut TokensIterator<'_>,
    context: &ExpandContext,
    source: &Text,
    origin: uuid::Uuid,
    syntax_type: impl ExpandExpression,
) -> Result<Vec<hir::Expression>, ShellError> {
    let mut exprs: Vec<hir::Expression> = vec![];

    loop {
        if token_nodes.at_end() {
            break;
        }

        let expr = expand_expr(&syntax_type, token_nodes, context)?;
        exprs.push(expr);
    }

    Ok(exprs)
}
