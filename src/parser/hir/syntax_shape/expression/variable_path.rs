use crate::parser::hir::syntax_shape::{
    expand_expr, expand_syntax, parse_single_node, AnyExpressionShape, BareShape, ExpandContext,
    ExpandExpression, ExpandSyntax, SkipSyntax, StringShape, TestSyntax, WhitespaceShape,
};
use crate::parser::{hir, hir::Expression, hir::TokensIterator, Operator, RawToken, TokenNode};
use crate::prelude::*;

#[derive(Debug, Copy, Clone)]
pub struct VariablePathShape;

impl ExpandExpression for VariablePathShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<hir::Expression, ShellError> {
        // 1. let the head be the first token, expecting a variable
        // 2. let the tail be an empty list of members
        // 2. while the next token (excluding ws) is a dot:
        //   1. consume the dot
        //   2. consume the next token as a member and push it onto tail

        let head = expand_expr(&VariableShape, token_nodes, context)?;
        let start = head.tag();
        let mut end = start;
        let mut tail: Vec<Tagged<String>> = vec![];

        loop {
            match DotShape.skip(token_nodes, context) {
                Err(_) => break,
                Ok(_) => {}
            }

            let syntax = expand_syntax(&MemberShape, token_nodes, context)?;
            let member = syntax.to_tagged_string(context.source);

            end = member.tag();
            tail.push(member);
        }

        Ok(hir::Expression::path(head, tail, start.until(end)))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PathTailShape;

impl ExpandSyntax for PathTailShape {
    type Output = (Vec<Tagged<String>>, Tag);
    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        let mut end: Option<Tag> = None;
        let mut tail = vec![];

        loop {
            match DotShape.skip(token_nodes, context) {
                Err(_) => break,
                Ok(_) => {}
            }

            let syntax = expand_syntax(&MemberShape, token_nodes, context)?;
            let member = syntax.to_tagged_string(context.source);
            end = Some(member.tag());
            tail.push(member);
        }

        match end {
            None => {
                return Err(ShellError::type_error(
                    "path tail",
                    token_nodes.typed_tag_at_cursor(),
                ))
            }

            Some(end) => Ok((tail, end)),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionContinuation {
    DotSuffix(Tag, Tagged<String>),
    InfixSuffix(Tagged<Operator>, Expression),
}

/// An expression continuation
#[derive(Debug, Copy, Clone)]
pub struct ExpressionContinuationShape;

impl ExpandSyntax for ExpressionContinuationShape {
    type Output = ExpressionContinuation;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<ExpressionContinuation, ShellError> {
        // Try to expand a `.`
        let dot = expand_syntax(&DotShape, token_nodes, context);

        match dot {
            // If a `.` was matched, it's a `Path`, and we expect a `Member` next
            Ok(dot) => {
                let syntax = expand_syntax(&MemberShape, token_nodes, context)?;
                let member = syntax.to_tagged_string(context.source);

                Ok(ExpressionContinuation::DotSuffix(dot, member))
            }

            // Otherwise, we expect an infix operator and an expression next
            Err(_) => {
                let (_, op, _) = expand_syntax(&InfixShape, token_nodes, context)?;
                let next = expand_expr(&AnyExpressionShape, token_nodes, context)?;

                Ok(ExpressionContinuation::InfixSuffix(op, next))
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct VariableShape;

impl ExpandExpression for VariableShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        _context: &ExpandContext,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(token_nodes, "variable", |token, token_tag| {
            Ok(match token {
                RawToken::Variable(tag) => hir::Expression::variable(tag, token_tag),
                _ => {
                    return Err(ShellError::type_error(
                        "variable",
                        token.type_name().tagged(token_tag),
                    ))
                }
            })
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Member {
    String(/* outer */ Tag, /* inner */ Tag),
    Bare(Tag),
}

impl Member {
    pub(crate) fn to_expr(&self) -> hir::Expression {
        match self {
            Member::String(outer, inner) => hir::Expression::string(inner, outer),
            Member::Bare(tag) => hir::Expression::string(tag, tag),
        }
    }

    pub(crate) fn to_tagged_string(&self, source: &str) -> Tagged<String> {
        match self {
            Member::String(outer, inner) => inner.string(source).tagged(outer),
            Member::Bare(tag) => tag.tagged_string(source),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MemberShape;

impl ExpandSyntax for MemberShape {
    type Output = Member;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<Member, ShellError> {
        let bare = BareShape.test(token_nodes, context);
        if let Some(peeked) = bare {
            let node = peeked.not_eof("member")?.commit();
            return Ok(Member::Bare(node.tag()));
        }

        let string = StringShape.test(token_nodes, context);

        if let Some(peeked) = string {
            let node = peeked.not_eof("member")?.commit();
            let (outer, inner) = node.expect_string();

            return Ok(Member::String(outer, inner));
        }

        Err(token_nodes.peek_any().type_error("member"))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DotShape;

impl SkipSyntax for DotShape {
    fn skip<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<(), ShellError> {
        expand_syntax(self, token_nodes, context)?;

        Ok(())
    }
}

impl ExpandSyntax for DotShape {
    type Output = Tag;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        _context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        parse_single_node(token_nodes, "dot", |token, token_tag| {
            Ok(match token {
                RawToken::Operator(Operator::Dot) => token_tag,
                _ => {
                    return Err(ShellError::type_error(
                        "dot",
                        token.type_name().tagged(token_tag),
                    ))
                }
            })
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct InfixShape;

impl ExpandSyntax for InfixShape {
    type Output = (Tag, Tagged<Operator>, Tag);

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        let checkpoint = token_nodes.checkpoint();

        // An infix operator must be prefixed by whitespace
        let start = expand_syntax(&WhitespaceShape, checkpoint.iterator, context)?;

        // Parse the next TokenNode after the whitespace
        let operator =
            parse_single_node(checkpoint.iterator, "infix operator", |token, token_tag| {
                Ok(match token {
                    // If it's an operator (and not `.`), it's a match
                    RawToken::Operator(operator) if operator != Operator::Dot => {
                        operator.tagged(token_tag)
                    }

                    // Otherwise, it's not a match
                    _ => {
                        return Err(ShellError::type_error(
                            "infix operator",
                            token.type_name().tagged(token_tag),
                        ))
                    }
                })
            })?;

        // An infix operator must be followed by whitespace
        let end = expand_syntax(&WhitespaceShape, checkpoint.iterator, context)?;

        checkpoint.commit();

        Ok((start, operator, end))
    }
}
