mod range;

use range::Bound::*;
use range::RangeArgument;
use std::fmt::Debug;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParserResult<'input, Output = ()> = Result<(&'input str, Output), &'input str>;

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

fn pair<'input, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'input, (R1, R2)>
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

fn map<'input, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'input, B>
    where
        P: Parser<'input, A>,
        F: Fn(A) -> B
{
    move |input| parser
        .parse(input)
        .map(|(rest, result)| (rest, map_fn(result)))
}

fn left<'input, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'input, R1>
    where
        P1: Parser<'input, R1> + 'input,
        P2: Parser<'input, R2> + 'input,
        R1: 'input,
        R2: 'input
{
    pair(parser1, parser2).map(|(left, _)| left)
}

fn right<'input, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'input, R2>
    where
        P1: Parser<'input, R1> + 'input,
        P2: Parser<'input, R2> + 'input,
        R1: 'input,
        R2: 'input
{
    pair(parser1, parser2).map(|(_, right)| right)
}

fn one_or_more<'input, P, R>(parser: P) -> impl Parser<'input, Vec<R>>
    where
        P: Parser<'input, R> + 'input,
        R: 'input
{
    parser.repeat(1..)
}

fn zero_or_more<'input, P, R>(parser: P) -> impl Parser<'input, Vec<R>>
    where
        P: Parser<'input, R> + 'input,
        R: 'input
{
    parser.repeat(0..)
}

fn any_char(input: &str) -> ParserResult<char> {
    match input.chars().next() {
        Some(char) => Ok((&input[char.len_utf8()..], char)),
        _ => Err(input)
    }
}

fn pred<'input, P, A, F>(parser: P, predicate: F) -> impl Parser<'input, A>
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

fn whitespace_char<'input>() -> impl Parser<'input, char> {
    any_char.pred(|c| c.is_whitespace())
}

fn space0<'input>() -> impl Parser<'input, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn space1<'input>() -> impl Parser<'input, Vec<char>> {
    one_or_more(whitespace_char())
}

fn whitespace_wrap<'input, P, R>(parser: P) -> impl Parser<'input, R>
    where
        P: Parser<'input, R> + 'input,
        R: 'input
{
    right(space0(), left(parser, space0()))
}

fn match_literal<'input>(expected: &'static str) -> impl Parser<'input> {
    move |input: &'input str| {
        match input.get(0..expected.len()) {
            Some(next) if next == expected => Ok((&input[expected.len()..], ())),
            _ => Err(input)
        }
    }
}

fn either<'input, P1, P2, R>(parser1: P1, parser2: P2) -> impl Parser<'input, R>
where
    P1: Parser<'input, R>,
    P2: Parser<'input, R>
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input)
    }
}

fn and_then<'input, P, R, NR, F, NP>(parser: P, func: F) -> impl Parser<'input, NR>
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

fn repeat<'input, P, R, O>(parser: P, range: R) -> impl Parser<'input, Vec<O>>
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

        Ok((input, items))
    }
}

fn identifier<'input>() -> impl Parser<'input, String> {
    let start = one_or_more(any_char.pred(|c| c.is_alphabetic()));
    let rest = zero_or_more(any_char.pred(|c| c.is_alphanumeric() || *c == '-'));

    pair(start, rest)
        .map(|(first, second)| first
            .iter()
            .chain(second.iter())
            .collect())
}

fn quoted_string<'input>() -> impl Parser<'input, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    ).map(|chars| chars.iter().collect())
}

fn attribute_pair<'input>() -> impl Parser<'input, (String, String)> {
    pair(
        identifier(),
        right(
            match_literal("="),
            quoted_string(),
        ),
    )
}

fn attributes<'input>() -> impl Parser<'input, Vec<(String, String)>> {
    zero_or_more(
        right(
            space1(),
            attribute_pair(),
        )
    )
}

fn element_start<'input>() -> impl Parser<'input, (String, Vec<(String, String)>)> {
    right(
        match_literal("<"),
        pair(
            identifier(),
            attributes(),
        ),
    )
}

fn element<'input>(end_literal: &'static str) -> impl Parser<'input, Element> {
    left(element_start(), match_literal(end_literal))
        .map(|(name, attributes)| Element {
            name,
            attributes,
            children: vec![],
        })
}

fn single_element<'input>() -> impl Parser<'input, Element> {
    element("/>")
}

fn open_element<'input>() -> impl Parser<'input, Element> {
    element(">")
}

fn single_or_open_element<'input>() -> impl Parser<'input, Element> {
    whitespace_wrap(either(single_element(), open_element()))
}

fn close_element<'input>(expected_name: String) -> impl Parser<'input, String> {
    let start = match_literal("</");
    let end = left(identifier(), match_literal(">"));
    let all_together = right(start, end);

    all_together.pred(move |name| name == &expected_name)
}

fn parent_element<'input>() -> impl Parser<'input, Element> {
    open_element().and_then(|el| {
        let children = left(
            one_or_more(single_or_open_element()),
            close_element(el.name.clone())
        );

        children.map(move |children| {
            let mut el = el.clone();
            el.children = children;
            el
        })
    })
}

#[cfg(test)]
mod tests {
    use crate::{match_literal, pair, right, one_or_more, zero_or_more, pred, any_char, identifier, quoted_string, attributes, single_element, open_element, Element, Parser, single_or_open_element, repeat};

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
    fn identifier_test() {
        let identifier = identifier();
        assert_eq!(
            Ok(("", "a1239ff".to_string())),
            identifier.parse("a1239ff")
        );
        assert_eq!(
            Ok((" some", "xxx-1-qq".to_string())),
            identifier.parse("xxx-1-qq some")
        );
        assert_eq!(
            Err("123-qqq"),
            identifier.parse("123-qqq")
        );
    }

    #[test]
    fn pair_combinator() {
        let tag_opener = pair(match_literal("<"), identifier());
        assert_eq!(
            Ok(("/>", ((), "my-first-element".to_string()))),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
    }

    #[test]
    fn right_combinator() {
        let tag_opener = right(match_literal("<"), identifier());
        assert_eq!(
            Ok(("/>", "my-first-element".to_string())),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
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
        let parser = repeat(match_literal("ha"), 1..);
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
        assert_eq!(Ok(("", vec![])), parser.parse(""));
    }

    #[test]
    fn quoted_string_parser() {
        assert_eq!(
            Ok(("", "Hello Joe!".to_string())),
            quoted_string().parse("\"Hello Joe!\"")
        );
    }

    #[test]
    fn attribute_parser() {
        assert_eq!(
            Ok((
                "",
                vec![
                    ("one".to_string(), "1".to_string()),
                    ("two".to_string(), "2".to_string()),
                ]
            )),
            attributes().parse(" one=\"1\" two=\"2\"")
        );
    }

    #[test]
    fn single_element_parser() {
        assert_eq!(
            Ok((
                "",
                Element {
                    name: "div".to_string(),
                    attributes: vec![("class".to_string(), "float".to_string())],
                    children: vec![],
                }
            )),
            single_element().parse("<div class=\"float\"/>")
        );
    }

    #[test]
    fn open_element_parser() {
        assert_eq!(
            Ok((
                "",
                Element {
                    name: "div".to_string(),
                    attributes: vec![("class".to_string(), "float".to_string())],
                    children: vec![],
                }
            )),
            open_element().parse("<div class=\"float\">")
        );
    }

    #[test]
    fn xml_parser() {
        let doc = r#"
        <top label="Top">
            <semi-bottom label="Bottom"/>
            <middle>
                <bottom label="Another bottom"/>
            </middle>
        </top>"#;
        let parsed_doc = Element {
            name: "top".to_string(),
            attributes: vec![("label".to_string(), "Top".to_string())],
            children: vec![
                Element {
                    name: "semi-bottom".to_string(),
                    attributes: vec![("label".to_string(), "Bottom".to_string())],
                    children: vec![],
                },
                Element {
                    name: "middle".to_string(),
                    attributes: vec![],
                    children: vec![Element {
                        name: "bottom".to_string(),
                        attributes: vec![("label".to_string(), "Another bottom".to_string())],
                        children: vec![],
                    }],
                },
            ],
        };
        assert_eq!(Ok(("", parsed_doc)), single_or_open_element().parse(doc));
    }

    #[test]
    fn mismatched_closing_tag() {
        let doc = r#"
        <top>
            <bottom/>
        </middle>"#;
        assert_eq!(Err("</middle>"), single_or_open_element().parse(doc));
    }
}
