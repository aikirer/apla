use std::{iter::Peekable, str::Chars};

use crate::{token::{Token, self}, spanned::Spanned};

pub struct Scanner<'a> {
    text: Peekable<Chars<'a>>,
    current: char,
    result: Vec<Spanned<Token>>,
    at: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            text: text.chars().peekable(),
            current: ' ',
            result: vec![],
            at: 0,
        }
    }

    pub fn scan(&mut self) -> &[Spanned<Token>] {
        while self.advance().is_some() {
            if self.skip_whitespace().is_none() { break; }
            use Token as T;
            let token = match self.current {
                '&' => self.token_if_next_else(vec![('&', T::And)], T::BitAnd),
                '|' => self.token_if_next_else(vec![('|', T::Or)], T::BitOr),
                '>' => self.token_if_next_else(vec![('>', T::BitRight), ('=', T::GreaterEqual)], T::Greater),
                '<' => self.token_if_next_else(vec![('<', T::BitLeft), ('=', T::SmallerEqual)], T::Smaller),
                '!' => self.token_if_next_else(vec![('=', T::NotEqual)], T::Bang),
                '+' => self.token_if_next_else(vec![('+', T::PlusPlus)], T::Plus),
                '-' => self.token_if_next_else(vec![('-', T::MinusMinus)], T::Minus),
                '*' => T::Star,
                '%' => T::Percent,
                '/' => T::Slash,
                '=' => self.token_if_next_else(vec![('=', T::EqualsEquals)], T::Equals),
                ':' => T::Colon,
                '.' => T::Dot,
                '(' => T::LeftParen,
                ')' => T::RightParen,
                '[' => T::LeftBracket,
                ']' => T::RightBracket,
                '{' => T::LeftBrace,
                '}' => T::RightBrace,
                '@' => T::At,
                ';' => T::Semicolon,
                c @ ('"' | '\'') => self.string(c),
                c if is_digit(c) => self.number(),
                c if is_alpha(c) => self.identifier(),
                _ => T::Unknown(self.current),
            };
            self.add(token);
        }
        self.add(Token::Eof);
        &self.result
    }

    fn collect_chars_while<F>(&mut self, mut f: F) -> String
    where
        F: FnMut(char) -> bool
    {
        let mut result = String::from(self.current);
        while let Some(c) = self.peek() {
            if !f(c) { break; }
            result.push(c);
            self.advance();
        }
        result
    }

    fn number(&mut self) -> Token {
        let mut found_dot = false;
        let str_num = self.collect_chars_while(|c| {
            if c == '.' {
                if found_dot { return false; }
                found_dot = true;
                return true;
            }
            is_digit(c)
        });
        match found_dot {
            true => Token::Float(str_num.parse().unwrap()),
            false => Token::Number(str_num.parse().unwrap()),
        }
    }

    fn identifier(&mut self) -> Token {
        token::str_to_reserved_or_ident(&self.collect_chars_while(is_alpha))
    }

    fn string(&mut self, closing_char: char) -> Token {
        self.advance(); // skip the opening char (")
        let s = Token::Str(self.collect_chars_while(|c| c != closing_char));
        self.advance();
        s
    }

    fn token_if_next_else(
        &mut self, wanted: Vec<(char, Token)>, on_false: Token
    ) -> Token
    {
        let peek = self.peek();
        let result = wanted.into_iter()
            .find(|c| peek == Some(c.0))
            .map(|c| c.1);
        if result.is_some() { self.advance(); }
        result.unwrap_or(on_false)
    }

    fn skip_whitespace(&mut self) -> Option<()> {
        while self.current.is_whitespace() {
            self.advance()?;
        }
        if self.current == '#' {
            while self.current != '\n' {
                self.advance()?;
            }
            self.advance();
        }
        Some(())
    }

    fn add(&mut self, token: Token) {
        let len = token.len();
        self.result.push(Spanned::new(token, self.at - len, len));
    }

    fn advance(&mut self) -> Option<char> {
        self.at += 1;
        self.current = self.text.next()?;
        Some(self.current)
    }

    fn peek(&mut self) -> Option<char> {
        self.text.peek().copied()
    }
}

fn is_alpha(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_digit(c: char) -> bool {
    c.is_numeric()
}
