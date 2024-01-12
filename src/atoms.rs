// This module simply defines the number encodings for all the atoms used in Programs-As-Data encoding

// this module has a lot of boilerplate that could be written much more succintly via macros
// however, the contents is unlikely to change and the expliclty written version is more obvious.

use std::{fmt::Display, str::FromStr};

use anyhow::bail;

#[repr(u8)]
pub enum Atom {
    Assign = 2,
    DoAssign = 3,
    While = 5,
    DoWhile = 7,
    If = 11,
    DoIf = 13,
    Var = 17,
    Quote = 19,
    Hd = 23,
    DoHd = 29,
    Tl = 31,
    DoTl = 37,
    Cons = 41,
    DoCons = 43,
}

impl TryFrom<u8> for Atom {
    type Error = anyhow::Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use Atom::*;
        Ok(match value {
            2 => Assign,
            3 => DoAssign,
            5 => While,
            7 => DoWhile,
            11 => If,
            13 => DoIf,
            17 => Var,
            19 => Quote,
            23 => Hd,
            29 => DoHd,
            31 => Tl,
            37 => DoTl,
            41 => Cons,
            43 => DoCons,
            _ => bail!("{value} does not encode an atom!"),
        })
    }
}

impl FromStr for Atom {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Atom::*;
        Ok(match s {
            "@:=" | "@asng" => Assign,
            "@doAsgn" => DoAssign,
            "@while" => While,
            "@doWhile" => DoWhile,
            "@if" => If,
            "@doIf" => DoIf,
            "@var" => Var,
            "@quote" => Quote,
            "@hd" => Hd,
            "@doHd" => DoHd,
            "@tl" => Tl,
            "@doTl" => DoTl,
            "@cons" => Cons,
            "@doCons" => DoCons,
            _ => bail!("{} is not a valid atom!", s),
        })
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Assign => "@:=",
            Atom::DoAssign => "@doAsgn",
            Atom::While => "@while",
            Atom::DoWhile => "@doWhile",
            Atom::If => "@if",
            Atom::DoIf => "@doIf",
            Atom::Var => "@var",
            Atom::Quote => "@quote",
            Atom::Hd => "@hd",
            Atom::DoHd => "@doHd",
            Atom::Tl => "@tl",
            Atom::DoTl => "@doTl",
            Atom::Cons => "@cons",
            Atom::DoCons => "@doCons",
        }
        .fmt(f)
    }
}
