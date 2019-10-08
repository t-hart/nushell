pub(crate) mod atom;
pub(crate) mod delimited;
pub(crate) mod file_path;
pub(crate) mod list;
pub(crate) mod number;
pub(crate) mod pattern;
pub(crate) mod string;
pub(crate) mod unit;
pub(crate) mod variable_path;

use crate::parser::hir::syntax_shape::{
    color_delimited_square, color_syntax, expand_atom, expand_delimited_square, expand_expr,
    expand_syntax, AtomicToken, BareShape, ColorSyntax, ColorableDotShape, DotShape, ExpandContext,
    ExpandExpression, ExpandSyntax, ExpansionRule, ExpressionContinuation,
    ExpressionContinuationShape, FlatShape,
};
use crate::parser::{
    hir,
    hir::{Expression, TokensIterator},
};
use crate::prelude::*;
use std::path::PathBuf;

#[derive(Debug, Copy, Clone)]
pub struct AnyExpressionShape;

impl ExpandExpression for AnyExpressionShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<hir::Expression, ShellError> {
        // Look for an expression at the cursor
        let head = expand_expr(&AnyExpressionStartShape, token_nodes, context)?;

        continue_expression(head, token_nodes, context)
    }
}

impl ColorSyntax for AnyExpressionShape {
    type Info = ();

    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) {
        // Look for an expression at the cursor
        color_syntax(&AnyExpressionStartShape, token_nodes, context, shapes);

        continue_coloring_expression(token_nodes, context, shapes);
    }
}

pub(crate) fn continue_expression(
    mut head: hir::Expression,
    token_nodes: &mut TokensIterator<'_>,
    context: &ExpandContext,
) -> Result<hir::Expression, ShellError> {
    loop {
        // Check to see whether there's any continuation after the head expression
        let continuation = expand_syntax(&ExpressionContinuationShape, token_nodes, context);

        match continuation {
            // If there's no continuation, return the head
            Err(_) => return Ok(head),
            // Otherwise, form a new expression by combining the head with the continuation
            Ok(continuation) => match continuation {
                // If the continuation is a `.member`, form a path with the new member
                ExpressionContinuation::DotSuffix(_dot, member) => {
                    head = Expression::dot_member(head, member);
                }

                // Otherwise, if the continuation is an infix suffix, form an infix expression
                ExpressionContinuation::InfixSuffix(op, expr) => {
                    head = Expression::infix(head, op, expr);
                }
            },
        }
    }
}

pub(crate) fn continue_coloring_expression(
    token_nodes: &mut TokensIterator<'_>,
    context: &ExpandContext,
    shapes: &mut Vec<Tagged<FlatShape>>,
) {
    loop {
        // Check to see whether there's any continuation after the head expression
        let (seen, _) = color_syntax(&ExpressionContinuationShape, token_nodes, context, shapes);

        if !seen || is_error(shapes) {
            return;
        }
    }
}

fn is_error(shapes: &mut Vec<Tagged<FlatShape>>) -> bool {
    let last = shapes.iter().last();

    match last {
        None => false,
        Some(Tagged {
            item: FlatShape::Error,
            ..
        }) => true,
        _ => false,
    }
}

#[derive(Debug, Copy, Clone)]
pub struct AnyExpressionStartShape;

impl ExpandExpression for AnyExpressionStartShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<hir::Expression, ShellError> {
        let atom = expand_atom(token_nodes, "expression", context, ExpansionRule::new())?;

        match atom.item {
            AtomicToken::Size { number, unit } => {
                return Ok(hir::Expression::size(
                    number.to_number(context.source),
                    unit.item,
                    atom.tag,
                ))
            }

            AtomicToken::SquareDelimited { nodes, .. } => {
                expand_delimited_square(&nodes, atom.tag, context)
            }

            AtomicToken::Word { .. } | AtomicToken::Dot { .. } => {
                let end = expand_syntax(&BareTailShape, token_nodes, context)?;
                Ok(hir::Expression::bare(atom.tag.until_option(end)))
            }

            other => return other.tagged(atom.tag).into_hir(context, "expression"),
        }
    }
}

impl ColorSyntax for AnyExpressionStartShape {
    type Info = ();
    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) {
        let atom = token_nodes.spanned(|token_nodes| {
            expand_atom(
                token_nodes,
                "expression",
                context,
                ExpansionRule::permissive(),
            )
        });

        let atom = match atom {
            Tagged {
                item: Err(err),
                tag,
            } => {
                shapes.push(FlatShape::Error.tagged(tag));
                return;
            }

            Tagged {
                item: Ok(value), ..
            } => value,
        };

        match atom.item {
            AtomicToken::Size { number, unit } => shapes.push(
                FlatShape::Size {
                    number: number.tag,
                    unit: unit.tag,
                }
                .tagged(atom.tag),
            ),

            AtomicToken::SquareDelimited { nodes, tags } => {
                color_delimited_square(tags, &nodes, atom.tag, context, shapes)
            }

            AtomicToken::Word { .. } | AtomicToken::Dot { .. } => {
                shapes.push(FlatShape::Word.tagged(atom.tag));
            }

            _ => atom.color_tokens(shapes),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct BareTailShape;

impl ColorSyntax for BareTailShape {
    type Info = ();
    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) -> Self::Info {
        loop {
            let (seen, _) = color_syntax(&BareShape, token_nodes, context, shapes);

            if seen {
                continue;
            }

            let (seen, _) = color_syntax(
                &ColorableDotShape { in_bare: true },
                token_nodes,
                context,
                shapes,
            );

            if seen {
                continue;
            } else {
                break;
            }
        }
    }
}

impl ExpandSyntax for BareTailShape {
    type Output = Option<Tag>;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Option<Tag>, ShellError> {
        let mut end: Option<Tag> = None;

        loop {
            match expand_syntax(&BareShape, token_nodes, context) {
                Ok(bare) => {
                    end = Some(bare.tag);
                    continue;
                }

                Err(_) => match expand_syntax(&DotShape, token_nodes, context) {
                    Ok(dot) => {
                        end = Some(dot);
                        continue;
                    }

                    Err(_) => break,
                },
            }
        }

        Ok(end)
    }
}

pub fn expand_file_path(string: &str, context: &ExpandContext) -> PathBuf {
    let expanded = shellexpand::tilde_with_context(string, || context.homedir());

    PathBuf::from(expanded.as_ref())
}
