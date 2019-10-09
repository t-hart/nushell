use crate::parser::hir::syntax_shape::{
    color_syntax, expand_atom, expand_expr, expand_syntax, parse_single_node, AnyExpressionShape,
    AtomicToken, BareShape, ColorSyntax, ExpandContext, ExpandExpression, ExpandSyntax,
    ExpansionRule, FlatShape, Peeked, SkipSyntax, StringShape, TestSyntax, WhitespaceShape,
};
use crate::parser::{hir, hir::Expression, hir::TokensIterator, Operator, RawToken};
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

impl ColorSyntax for VariablePathShape {
    type Info = ();
    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) -> Self::Info {
        color_syntax(&VariableShape, token_nodes, context, shapes);

        loop {
            let (seen, _) = color_syntax(
                &ColorableDotShape { in_bare: false },
                token_nodes,
                context,
                shapes,
            );

            if !seen {
                break;
            }

            color_syntax(&MemberShape, token_nodes, context, shapes);
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PathTailShape;

impl ColorSyntax for PathTailShape {
    type Info = ();
    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) -> Self::Info {
        loop {
            let (seen, _) = color_syntax(
                &ColorableDotShape { in_bare: false },
                token_nodes,
                context,
                shapes,
            );

            if !seen {
                return;
            }

            let (seen, _) = color_syntax(&MemberShape, token_nodes, context, shapes);

            if !seen {
                return;
            }
        }
    }
}

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

pub enum ContinuationInfo {
    Dot,
    Infix,
    End,
}

impl ColorSyntax for ExpressionContinuationShape {
    type Info = ContinuationInfo;
    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) -> ContinuationInfo {
        if token_nodes.at_end_possible_ws() {
            return ContinuationInfo::End;
        }

        let checkpoint = token_nodes.checkpoint();

        // Try to expand a `.`
        let (seen, _) = color_syntax(
            &ColorableDotShape { in_bare: false },
            checkpoint.iterator,
            context,
            shapes,
        );

        if seen {
            let (seen, _) = color_syntax(&MemberShape, checkpoint.iterator, context, shapes);

            if seen {
                checkpoint.commit();
                ContinuationInfo::Dot
            } else {
                ContinuationInfo::End
            }
        } else {
            let (seen, _) = color_syntax(&InfixShape, checkpoint.iterator, context, shapes);

            if !seen {
                return ContinuationInfo::End;
            }

            let (seen, _) = color_syntax(&AnyExpressionShape, checkpoint.iterator, context, shapes);

            if seen {
                checkpoint.commit();
                ContinuationInfo::Infix
            } else {
                ContinuationInfo::End
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
        context: &ExpandContext,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(token_nodes, "variable", |token, token_tag, _| {
            Ok(match token {
                RawToken::Variable(tag) => {
                    if tag.slice(context.source) == "it" {
                        hir::Expression::it_variable(tag, token_tag)
                    } else {
                        hir::Expression::variable(tag, token_tag)
                    }
                }
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

impl ColorSyntax for VariableShape {
    type Info = ();
    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) {
        let atom = expand_atom(
            token_nodes,
            "variable",
            context,
            ExpansionRule::permissive(),
        );

        let atom = match atom {
            Err(_) => return,
            Ok(atom) => atom,
        };

        match atom.item {
            AtomicToken::Variable { name: _ } => shapes.push(FlatShape::Variable.tagged(atom.tag)),
            AtomicToken::ItVariable { name: _ } => {
                shapes.push(FlatShape::ItVariable.tagged(atom.tag))
            }
            _other => return,
        }
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

    pub(crate) fn tag(&self) -> Tag {
        match self {
            Member::String(outer, _inner) => *outer,
            Member::Bare(tag) => *tag,
        }
    }

    pub(crate) fn to_tagged_string(&self, source: &str) -> Tagged<String> {
        match self {
            Member::String(outer, inner) => inner.string(source).tagged(outer),
            Member::Bare(tag) => tag.tagged_string(source),
        }
    }

    pub(crate) fn tagged_type_name(&self) -> Tagged<&'static str> {
        match self {
            Member::String(outer, _inner) => "string".tagged(outer),
            Member::Bare(tag) => "word".tagged(tag),
        }
    }
}

enum ColumnPathState {
    Initial,
    LeadingDot(Tag),
    Dot(Tag, Vec<Member>, Tag),
    Member(Tag, Vec<Member>),
    Error(ShellError),
}

impl ColumnPathState {
    pub fn dot(self, dot: Tag) -> ColumnPathState {
        match self {
            ColumnPathState::Initial => ColumnPathState::LeadingDot(dot),
            ColumnPathState::LeadingDot(_) => {
                ColumnPathState::Error(ShellError::type_error("column", "dot".tagged(dot)))
            }
            ColumnPathState::Dot(..) => {
                ColumnPathState::Error(ShellError::type_error("column", "dot".tagged(dot)))
            }
            ColumnPathState::Member(tag, members) => ColumnPathState::Dot(tag, members, dot),
            ColumnPathState::Error(err) => ColumnPathState::Error(err),
        }
    }

    pub fn member(self, member: Member) -> ColumnPathState {
        match self {
            ColumnPathState::Initial => ColumnPathState::Member(member.tag(), vec![member]),
            ColumnPathState::LeadingDot(tag) => {
                ColumnPathState::Member(tag.until(member.tag()), vec![member])
            }

            ColumnPathState::Dot(tag, mut tags, _) => {
                ColumnPathState::Member(tag.until(member.tag()), {
                    tags.push(member);
                    tags
                })
            }
            ColumnPathState::Member(..) => {
                ColumnPathState::Error(ShellError::type_error("column", member.tagged_type_name()))
            }
            ColumnPathState::Error(err) => ColumnPathState::Error(err),
        }
    }

    pub fn into_path(self, next: Peeked) -> Result<Tagged<Vec<Member>>, ShellError> {
        match self {
            ColumnPathState::Initial => Err(next.type_error("column path")),
            ColumnPathState::LeadingDot(dot) => {
                Err(ShellError::type_error("column", "dot".tagged(dot)))
            }
            ColumnPathState::Dot(_tag, _members, dot) => {
                Err(ShellError::type_error("column", "dot".tagged(dot)))
            }
            ColumnPathState::Member(tag, tags) => Ok(tags.tagged(tag)),
            ColumnPathState::Error(err) => Err(err),
        }
    }
}

pub fn expand_column_path<'a, 'b>(
    token_nodes: &'b mut TokensIterator<'a>,
    context: &ExpandContext,
) -> Result<Tagged<Vec<Member>>, ShellError> {
    let mut state = ColumnPathState::Initial;

    loop {
        let member = MemberShape.expand_syntax(token_nodes, context);

        match member {
            Err(_) => break,
            Ok(member) => state = state.member(member),
        }

        let dot = DotShape.expand_syntax(token_nodes, context);

        match dot {
            Err(_) => break,
            Ok(dot) => state = state.dot(dot),
        }
    }

    state.into_path(token_nodes.peek_non_ws())
}

#[derive(Debug, Copy, Clone)]
pub struct ColumnPathShape;

impl ColorSyntax for ColumnPathShape {
    type Info = ();
    fn color_syntax<'a, 'b>(
        &self,
        _token_nodes: &'b mut TokensIterator<'a>,
        _context: &ExpandContext,
        _shapes: &mut Vec<Tagged<FlatShape>>,
    ) -> Self::Info {
        unimplemented!()
    }
}

impl ExpandSyntax for ColumnPathShape {
    type Output = Tagged<Vec<Member>>;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        expand_column_path(token_nodes, context)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MemberShape;

impl ColorSyntax for MemberShape {
    type Info = ();

    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) -> Self::Info {
        let bare = BareShape.test(token_nodes, context);
        if let Some(peeked) = bare {
            let peeked = peeked.not_eof("column");

            match peeked {
                Err(_) => return,
                Ok(peeked) => {
                    let tag = peeked.node.tag();
                    peeked.commit();
                    return shapes.push(FlatShape::BareMember.tagged(tag));
                }
            }
        }

        let string = StringShape.test(token_nodes, context);

        if let Some(peeked) = string {
            let peeked = peeked.not_eof("column");

            match peeked {
                Err(_) => return,
                Ok(peeked) => {
                    let tag = peeked.node.tag();
                    peeked.commit();
                    return shapes.push(FlatShape::BareMember.tagged(tag));
                }
            }
        }
    }
}

impl ExpandSyntax for MemberShape {
    type Output = Member;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<Member, ShellError> {
        let bare = BareShape.test(token_nodes, context);
        if let Some(peeked) = bare {
            let node = peeked.not_eof("column")?.commit();
            return Ok(Member::Bare(node.tag()));
        }

        let string = StringShape.test(token_nodes, context);

        if let Some(peeked) = string {
            let node = peeked.not_eof("column")?.commit();
            let (outer, inner) = node.expect_string();

            return Ok(Member::String(outer, inner));
        }

        Err(token_nodes.peek_any().type_error("column"))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DotShape;

#[derive(Debug, Copy, Clone)]
pub struct ColorableDotShape {
    pub(crate) in_bare: bool,
}

impl ColorSyntax for ColorableDotShape {
    type Info = ();
    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        _context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) -> Self::Info {
        let peeked = token_nodes.peek_any().not_eof("dot");

        let peeked = match peeked {
            Err(_) => return,
            Ok(peeked) => peeked,
        };

        match peeked.node {
            node if node.is_dot() => {
                peeked.commit();

                if self.in_bare {
                    shapes.push(FlatShape::Word.tagged(node.tag()))
                } else {
                    shapes.push(FlatShape::Dot.tagged(node.tag()))
                }
            }

            _ => return,
        }
    }
}

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
        parse_single_node(token_nodes, "dot", |token, token_tag, _| {
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

impl ColorSyntax for InfixShape {
    type Info = ();
    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        outer_shapes: &mut Vec<Tagged<FlatShape>>,
    ) -> Self::Info {
        let checkpoint = token_nodes.checkpoint();
        let mut shapes = vec![];

        // An infix operator must be prefixed by whitespace
        let (seen, _) = color_syntax(&WhitespaceShape, checkpoint.iterator, context, &mut shapes);

        if !seen {
            return;
        }

        // Parse the next TokenNode after the whitespace
        let operator = parse_single_node(
            checkpoint.iterator,
            "infix operator",
            |token, token_tag, _| {
                Ok(match token {
                    // If it's an operator (and not `.`), it's a match
                    RawToken::Operator(operator) if operator != Operator::Dot => {
                        shapes.push(FlatShape::Operator.tagged(token_tag))
                    }

                    // Otherwise, it's not a match
                    _ => {}
                })
            },
        );

        match operator {
            Err(_) => return,
            Ok(_) => {}
        }

        // An infix operator must be followed by whitespace
        let (seen, _) = color_syntax(&WhitespaceShape, checkpoint.iterator, context, &mut shapes);

        if !seen {
            return;
        }

        outer_shapes.extend(shapes);
        checkpoint.commit();
    }
}

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
        let operator = parse_single_node(
            checkpoint.iterator,
            "infix operator",
            |token, token_tag, _| {
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
            },
        )?;

        // An infix operator must be followed by whitespace
        let end = expand_syntax(&WhitespaceShape, checkpoint.iterator, context)?;

        checkpoint.commit();

        Ok((start, operator, end))
    }
}
