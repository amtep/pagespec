//! A module for parsing ikiwiki's `PageSpec` expressions into abstract syntax trees.

/* Copyright (C) 2024  Richard Braakman

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

use winnow::ascii::{digit1, space1};
use winnow::combinator::{alt, cut_err, delimited, preceded, separated, trace};
pub use winnow::error::{ContextError, ParseError};
use winnow::token::{take_till, take_while};
use winnow::{PResult, Parser};

/// A glob expression that matches pages or any other files.
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct FileGlob(String);

/// A glob expression that matches only pages.
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct PageGlob(String);

/// A glob expression that matches page tags.
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct TagGlob(String);

/// A glob expression that matches a metadata string.
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct StringGlob(String);

/// A glob expression that matches usernames.
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct UserGlob(String);

/// A glob expression that matches IP addresses.
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct IpGlob(String);

/// A non-globbed page name.
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Page(String);

/// The abstract level of pagespec expressions, representing the various ways they can be combined.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    MatchesAny(Vec<Expression>),
    MatchesAll(Vec<Expression>),
    DoesNotMatch(Box<Expression>),
    Matches(Box<Expression>),
    MatchType(MatchType),
}

/// The concrete level of pagespec expressions, representing the various matching operations available.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MatchType {
    /// Pages or files whose name matches the glob.
    Name(FileGlob),
    /// Pages whose name matches the glob.
    PageName(PageGlob),
    /// Pages that link to a page whose name matches the glob.
    LinksTo(PageGlob),
    /// Pages that have a tag that matches the glob.
    PageTagged(TagGlob),
    /// Pages that are linked from the given page.
    LinkedFrom(Page),
    /// Files that have been created in the given month number.
    CreationMonth(usize),
    /// Files that have been created on the given day of the month.
    CreationDay(usize),
    /// Files that have been created in the given year.
    CreationYear(usize),
    /// Files that have been created after the given page.
    CreatedAfter(Page),
    /// Files that have been created before the given page.
    CreatedBefore(Page),
    /// Like `Name`, but matches even internal-use pages that don't normally match globs.
    InternalGlob(FileGlob),
    /// Pages that have a title matching the glob.
    PageTitle(StringGlob),
    /// Pages that have an author matching the glob.
    PageAuthor(StringGlob),
    /// Pages that have an author url matching the glob.
    PageAuthorUrl(StringGlob),
    /// Pages that have a license matching the glob.
    PageLicense(StringGlob),
    /// Pages that have a copyright statement matching the glob.
    PageCopyright(StringGlob),
    /// Pages that have a GUID matching the glob.
    PageGuid(StringGlob),
    /// Modifications made by a user whose name matches the glob.
    User(UserGlob),
    /// Modifications made by a wiki admin.
    Admin,
    /// Modifications made from an IP address that matches the glob.
    IpAddress(IpGlob),
    /// Comments made to a page whose name matches the glob.
    Comment(PageGlob),
    /// Pending comments made to a page whose name matches the glob.
    PendingComment(PageGlob),
    /// Comments being posted to a page whose name matches the glob.
    PostComment(PageGlob),
}

/// Swallow whitespace.
fn ws(input: &mut &str) -> PResult<()> {
    trace("ws", space1.void()).parse_next(input)
}

/// A word that may contain glob markers `*` and `?`.
/// Since can be used to match filenames, be forgiving about what it may contain.
fn glob(input: &mut &str) -> PResult<String> {
    trace(
        "glob",
        take_till(1.., (|c: char| c.is_ascii_whitespace(), b"!()")).map(str::to_owned),
    )
    .parse_next(input)
}

/// A page name without glob markers.
fn pagename(input: &mut &str) -> PResult<String> {
    trace(
        "pagename",
        take_till(1.., (|c: char| c.is_ascii_whitespace(), b"!()?*")).map(str::to_owned),
    )
    .parse_next(input)
}

/// An IP address that may contain glob markers `*` and `?`.
fn ipglob(input: &mut &str) -> PResult<String> {
    // Accept IPv4 or IPv6 addresses
    trace(
        "ipglob",
        alt((
            take_while(1.., ('0'..='9', b".?*")),
            take_while(1.., ('0'..='9', b":?*")),
        ))
        .map(str::to_owned),
    )
    .parse_next(input)
}

/// The number of a month, so must be between 1 and 12 inclusive
fn monthnum(input: &mut &str) -> PResult<usize> {
    trace(
        "monthnum",
        digit1.parse_to().verify(|n: &usize| (1..=12).contains(n)),
    )
    .parse_next(input)
}

/// The day in a month, so must be between 1 and 31 inclusive
fn monthday(input: &mut &str) -> PResult<usize> {
    trace(
        "monthday",
        digit1.parse_to().verify(|n: &usize| (1..=31).contains(n)),
    )
    .parse_next(input)
}

/// A year number, no restrictions on range except fits in usize.
fn year(input: &mut &str) -> PResult<usize> {
    trace("year", digit1.parse_to()).parse_next(input)
}

/// A single match term.
fn terminal(input: &mut &str) -> PResult<MatchType> {
    // This `alt` has more alternatives than winnow supports, so it's restructured with
    // a secondary `alt` as its final alternative.
    trace(
        "terminal",
        alt((
            delimited("glob(", cut_err(glob.map(FileGlob)), ')').map(MatchType::Name),
            delimited("page(", cut_err(glob.map(PageGlob)), ')').map(MatchType::PageName),
            delimited("link(", cut_err(glob.map(PageGlob)), ')').map(MatchType::LinksTo),
            delimited("tagged(", cut_err(glob.map(TagGlob)), ')').map(MatchType::PageTagged),
            delimited("backlink(", cut_err(pagename.map(Page)), ')').map(MatchType::LinkedFrom),
            delimited("creation_month(", cut_err(monthnum), ')').map(MatchType::CreationMonth),
            delimited("creation_day(", cut_err(monthday), ')').map(MatchType::CreationDay),
            delimited("creation_year(", cut_err(year), ')').map(MatchType::CreationYear),
            delimited("created_after(", cut_err(pagename.map(Page)), ')')
                .map(MatchType::CreatedAfter),
            delimited("created_before(", cut_err(pagename.map(Page)), ')')
                .map(MatchType::CreatedBefore),
            alt((
                delimited("internal(", cut_err(glob.map(FileGlob)), ')')
                    .map(MatchType::InternalGlob),
                delimited("title(", cut_err(glob.map(StringGlob)), ')').map(MatchType::PageTitle),
                delimited("author(", cut_err(glob.map(StringGlob)), ')').map(MatchType::PageAuthor),
                delimited("authorurl(", cut_err(glob.map(StringGlob)), ')')
                    .map(MatchType::PageAuthorUrl),
                delimited("license(", cut_err(glob.map(StringGlob)), ')')
                    .map(MatchType::PageLicense),
                delimited("copyright(", cut_err(glob.map(StringGlob)), ')')
                    .map(MatchType::PageCopyright),
                delimited("guid(", cut_err(glob.map(StringGlob)), ')').map(MatchType::PageGuid),
                delimited("user(", cut_err(glob.map(UserGlob)), ')').map(MatchType::User),
                "admin()".value(MatchType::Admin),
                delimited("ip(", cut_err(ipglob.map(IpGlob)), ')').map(MatchType::IpAddress),
                delimited("comment(", cut_err(glob.map(PageGlob)), ')').map(MatchType::Comment),
                delimited("comment_pending(", cut_err(glob.map(PageGlob)), ')')
                    .map(MatchType::PendingComment),
                delimited("postcomment(", cut_err(glob.map(PageGlob)), ')')
                    .map(MatchType::PostComment),
                glob.map(FileGlob).map(MatchType::Name),
            )),
        )),
    )
    .parse_next(input)
}

/// A single part of a potential `and` or `or` sequence.
fn expression_term(input: &mut &str) -> PResult<Expression> {
    trace(
        "expression_term",
        alt((
            terminal.map(Expression::MatchType),
            delimited('(', cut_err(expression), ')')
                .map(Box::new)
                .map(Expression::Matches),
            preceded('!', cut_err(expression_term))
                .map(Box::new)
                .map(Expression::DoesNotMatch),
        )),
    )
    .parse_next(input)
}

fn expression(input: &mut &str) -> PResult<Expression> {
    trace(
        "expression",
        alt((
            separated(2.., expression_term, (ws, "and", ws)).map(Expression::MatchesAll),
            separated(2.., expression_term, (ws, "or", ws)).map(Expression::MatchesAny),
            expression_term,
        )),
    )
    .parse_next(input)
}
pub fn parse(input: &str) -> Result<Expression, ParseError<&str, ContextError>> {
    expression.parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let result = parse("apagename");
        assert_eq!(
            result,
            Ok(Expression::MatchType(MatchType::Name(FileGlob(
                "apagename".to_string()
            ))))
        );
    }

    #[test]
    fn pageglob() {
        let result = parse("page(*foo)");
        assert_eq!(
            result,
            Ok(Expression::MatchType(MatchType::PageName(PageGlob(
                "*foo".to_string()
            ))))
        );
    }

    #[test]
    fn bad_day() {
        let result = parse("creation_day(12)");
        assert!(result.is_ok());
        let result = parse("creation_day(42)");
        assert!(result.is_err());
    }

    #[test]
    fn negated() {
        let result = parse("!creation_year(2018)");
        assert_eq!(
            result,
            Ok(Expression::DoesNotMatch(Box::new(Expression::MatchType(
                MatchType::CreationYear(2018)
            ))))
        );
    }

    #[test]
    fn negated_group() {
        let result = parse("!(foo or bar)");
        assert_eq!(
            result,
            Ok(Expression::DoesNotMatch(Box::new(Expression::Matches(
                Box::new(Expression::MatchesAny(vec![
                    Expression::MatchType(MatchType::Name(FileGlob("foo".to_string()))),
                    Expression::MatchType(MatchType::Name(FileGlob("bar".to_string())))
                ]))
            ))))
        );
    }

    #[test]
    fn and_expr() {
        let result = parse("foo and admin()");
        assert_eq!(
            result,
            Ok(Expression::MatchesAll(vec![
                Expression::MatchType(MatchType::Name(FileGlob("foo".to_string()))),
                Expression::MatchType(MatchType::Admin)
            ]))
        );
    }

    #[test]
    fn or_expr() {
        let result = parse("foo or admin()");
        assert_eq!(
            result,
            Ok(Expression::MatchesAny(vec![
                Expression::MatchType(MatchType::Name(FileGlob("foo".to_string()))),
                Expression::MatchType(MatchType::Admin)
            ]))
        );
    }

    #[test]
    fn complex() {
        let result = parse("(foo or !bar) and !(moo or oom)");
        assert_eq!(
            result,
            Ok(Expression::MatchesAll(vec![
                Expression::Matches(Box::new(Expression::MatchesAny(vec![
                    Expression::MatchType(MatchType::Name(FileGlob("foo".to_string()))),
                    Expression::DoesNotMatch(Box::new(Expression::MatchType(MatchType::Name(
                        FileGlob("bar".to_string())
                    ))))
                ]))),
                Expression::DoesNotMatch(Box::new(Expression::Matches(Box::new(
                    Expression::MatchesAny(vec![
                        Expression::MatchType(MatchType::Name(FileGlob("moo".to_string()))),
                        Expression::MatchType(MatchType::Name(FileGlob("oom".to_string())))
                    ])
                ))))
            ]))
        );
    }

    #[test]
    fn dangling_and() {
        let result = parse("foo and");
        assert!(result.is_err());
        let result = parse("foo and ");
        assert!(result.is_err());
        let result = parse("and foo");
        assert!(result.is_err());
        let result = parse(" and foo");
        assert!(result.is_err());
    }

    #[test]
    fn dangling_or() {
        let result = parse("foo or");
        assert!(result.is_err());
        let result = parse("foo or ");
        assert!(result.is_err());
        let result = parse("or foo");
        assert!(result.is_err());
        let result = parse(" or foo");
        assert!(result.is_err());
    }
}
