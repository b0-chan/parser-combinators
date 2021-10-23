use std::fmt::Debug;
use crate::range::RangeArgument;
use crate:: range::Bound::*;


pub type ParserResult<'input, Output = ()> = Result<(&'input str, Output), &'input str>;

pub trait Parser<'input, Output = ()> {
    fn parse(&self, input: &'input str) -> ParserResult<'input, Output>;

    fn map<F, NR>(self, map_fn: F) -> BoxedParser<'input, NR>
        where
            Self: Sized + 'input,
            Output: 'input,
            NR: 'input,
            F: Fn(Output) -> NR + 'input
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'input, Output>
        where
            Self: Sized + 'input,
            Output: 'input,
            F: Fn(&Output) -> bool + 'input
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    fn and_then<F, NP, NR>(self, f: F) -> BoxedParser<'input, NR>
        where
            Self: Sized + 'input,
            Output: 'input,
            NR: 'input,
            NP: Parser<'input, NR> + 'input,
            F: Fn(Output) -> NP + 'input,
    {
        BoxedParser::new(and_then(self, f))
    }

    fn repeat<R>(self, range: R) -> BoxedParser<'input, Vec<Output>>
        where
            Self: Sized + 'input,
            Output: 'input,
            R: RangeArgument<usize> + Debug + 'input
    {
        BoxedParser::new(repeat(self, range))
    }
}

impl<'input, F, Output> Parser<'input, Output> for F
    where
        F: Fn(&'input str) -> ParserResult<'input, Output>
{
    fn parse(&self, input: &'input str) -> ParserResult<'input, Output> {
        self(input)
    }
}

pub struct BoxedParser<'input, Output> (Box<dyn Parser<'input, Output> + 'input>);

impl<'input, Output> BoxedParser<'input, Output> {
    fn new<P>(parser: P) -> Self
        where
            P: Parser<'input, Output> + 'input
    {
        Self(Box::new(parser))
    }
}

impl<'input, Output> Parser<'input, Output> for BoxedParser<'input, Output> {
    fn parse(&self, input: &'input str) -> ParserResult<'input, Output> {
        self.0.parse(input)
    }
}

pub fn pair<'input, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'input, (R1, R2)>
    where
        P1: Parser<'input, R1>,
        P2: Parser<'input, R2>
{
    move |input| {
        let (rest, result1) = parser1.parse(input)?;
        let (rest, result2) = parser2.parse(rest)?;

        Ok((rest, (result1, result2)))
    }
}

pub fn map<'input, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'input, B>
    where
        P: Parser<'input, A>,
        F: Fn(A) -> B
{
    move |input| parser
        .parse(input)
        .map(|(rest, result)| (rest, map_fn(result)))
}

pub fn left<'input, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'input, R1>
    where
        P1: Parser<'input, R1> + 'input,
        P2: Parser<'input, R2> + 'input,
        R1: 'input,
        R2: 'input
{
    pair(parser1, parser2).map(|(left, _)| left)
}

pub fn right<'input, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'input, R2>
    where
        P1: Parser<'input, R1> + 'input,
        P2: Parser<'input, R2> + 'input,
        R1: 'input,
        R2: 'input
{
    pair(parser1, parser2).map(|(_, right)| right)
}

pub fn one_or_more<'input, P, R>(parser: P) -> impl Parser<'input, Vec<R>>
    where
        P: Parser<'input, R> + 'input,
        R: 'input
{
    parser.repeat(1..)
}

pub fn zero_or_more<'input, P, R>(parser: P) -> impl Parser<'input, Vec<R>>
    where
        P: Parser<'input, R> + 'input,
        R: 'input
{
    parser.repeat(0..)
}

pub fn any_char(input: &str) -> ParserResult<char> {
    match input.chars().next() {
        Some(char) => Ok((&input[char.len_utf8()..], char)),
        _ => Err(input)
    }
}

pub fn pred<'input, P, A, F>(parser: P, predicate: F) -> impl Parser<'input, A>
    where
        P: Parser<'input, A>,
        F: Fn(&A) -> bool,
{
    move |input| {
        match parser.parse(input) {
            Ok((next_input, value)) if predicate(&value) => Ok((next_input, value)),
            _ => Err(input)
        }
    }
}

pub fn whitespace_char<'input>() -> impl Parser<'input, char> {
    any_char.pred(|c| c.is_whitespace())
}

pub fn space0<'input>() -> impl Parser<'input, Vec<char>> {
    zero_or_more(whitespace_char())
}

pub fn space1<'input>() -> impl Parser<'input, Vec<char>> {
    one_or_more(whitespace_char())
}

pub fn whitespace_wrap<'input, P, R>(parser: P) -> impl Parser<'input, R>
    where
        P: Parser<'input, R> + 'input,
        R: 'input
{
    right(space0(), left(parser, space0()))
}

pub fn match_literal<'input>(expected: &'static str) -> impl Parser<'input> {
    move |input: &'input str| {
        match input.get(0..expected.len()) {
            Some(next) if next == expected => Ok((&input[expected.len()..], ())),
            _ => Err(input)
        }
    }
}

pub fn either<'input, P1, P2, R>(parser1: P1, parser2: P2) -> impl Parser<'input, R>
    where
        P1: Parser<'input, R>,
        P2: Parser<'input, R>
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input)
    }
}

pub fn and_then<'input, P, R, NR, F, NP>(parser: P, func: F) -> impl Parser<'input, NR>
    where
        P: Parser<'input, R>,
        NP: Parser<'input, NR>,
        F: Fn(R) -> NP
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => func(result).parse(next_input),
        Err(err) => Err(err)
    }
}

pub fn repeat<'input, P, R, O>(parser: P, range: R) -> impl Parser<'input, Vec<O>>
    where
        P: Parser<'input, O>,
        R: RangeArgument<usize> + Debug + 'input
{
    move |mut input| {
        let mut items = vec![];

        loop {
            match range.end() {
                Included(&max_items_count) => {
                    if items.len() >= max_items_count {
                        break;
                    }
                }
                Excluded(&max_items_count) => {
                    if items.len() + 1 >= max_items_count {
                        break;
                    }
                }
                Unbounded => ()
            }

            if let Ok((rest, item)) = parser.parse(input) {
                items.push(item);
                input = rest
            } else {
                break;
            }
        }

        match range.start() {
            Included(&min_count) if items.len() < min_count => Err(input),
            _ => Ok((input, items))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{match_literal, pair, right, one_or_more, zero_or_more, pred, any_char, Parser, repeat};

    #[test]
    fn literal_parser() {
        let parse_joe = match_literal("Hello Joe!");
        assert_eq!(
            Ok(("", ())),
            parse_joe.parse("Hello Joe!")
        );
        assert_eq!(
            Ok((" Hello Robert!", ())),
            parse_joe.parse("Hello Joe! Hello Robert!")
        );
        assert_eq!(
            Err("Hello Mike!"),
            parse_joe.parse("Hello Mike!")
        );
    }

    #[test]
    fn pair_combinator() {
        let empty_tag = pair(match_literal("<"), match_literal("/>"));
        assert_eq!(
            Ok(("", ((), ()))),
            empty_tag.parse("</>")
        );
        assert_eq!(Err("oops"), empty_tag.parse("oops"));
        assert_eq!(Err("!oops"), empty_tag.parse("<!oops"));
    }

    #[test]
    fn right_combinator() {
        let empty_tag = right(
            match_literal("<"),
            match_literal("/>")
        );
        assert_eq!(
            Ok(("", ())),
            empty_tag.parse("</>")
        );
        assert_eq!(Err("oops"), empty_tag.parse("oops"));
        assert_eq!(Err("!oops"), empty_tag.parse("<!oops"));
    }

    #[test]
    fn one_or_more_combinator() {
        let parser = one_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Err("ahah"), parser.parse("ahah"));
        assert_eq!(Err(""), parser.parse(""));
    }

    #[test]
    fn zero_or_more_combinator() {
        let parser = zero_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
        assert_eq!(Ok(("", vec![])), parser.parse(""));
    }

    #[test]
    fn predicate_combinator() {
        let parser = pred(any_char, |c| *c == 'o');
        assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
        assert_eq!(Err("lol"), parser.parse("lol"));
    }

    #[test]
    fn repeat_test() {
        let parser1 = repeat(match_literal("ha"), 1..);
        let parser0 = repeat(match_literal("ha"), 0..);

        assert_eq!(Ok(("", vec![(), (), ()])), parser1.parse("hahaha"));
        assert_eq!(Err("ahah"), parser1.parse("ahah"));
        assert_eq!(Err(""), parser1.parse(""));

        assert_eq!(Ok(("", vec![(), (), ()])), parser0.parse("hahaha"));
        assert_eq!(Ok(("ahah", vec![])), parser0.parse("ahah"));
        assert_eq!(Err(""), parser1.parse(""));
    }
}