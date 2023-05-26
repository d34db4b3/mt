use std::{
    collections::HashSet,
    fmt::{self, Display, Write},
    iter, mem, ops,
};

#[derive(PartialEq, Debug, Clone, PartialOrd, Eq, Ord)]
enum Value {
    Val(bool),
    Var(String),
}

impl Value {
    fn is_true(&self) -> bool {
        match self {
            Value::Val(true) => true,
            _ => false,
        }
    }

    fn is_false(&self) -> bool {
        match self {
            Value::Val(false) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Val(true) => f.write_char('1'),
            Value::Val(false) => f.write_char('0'),
            Value::Var(s) => f.write_str(s),
        }
    }
}

#[derive(PartialEq, Debug, Clone, PartialOrd, Eq, Ord)]
enum Expr {
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(Box<Expr>),
    Xor(Vec<Expr>),
    Value(Value),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::And(exprs) => {
                for (i, x) in exprs.iter().enumerate() {
                    if i != 0 {
                        f.write_char('&')?;
                    }
                    x.sub_fmt(f)?;
                }
            }
            Expr::Not(expr) => {
                f.write_char('!')?;
                expr.sub_fmt(f)?;
            }
            Expr::Xor(exprs) => {
                for (i, x) in exprs.iter().enumerate() {
                    if i != 0 {
                        f.write_char('^')?;
                    }
                    x.sub_fmt(f)?;
                }
            }
            Expr::Value(v) => v.fmt(f)?,
            Expr::Or(exprs) => {
                for (i, x) in exprs.iter().enumerate() {
                    if i != 0 {
                        f.write_char('|')?;
                    }
                    x.sub_fmt(f)?;
                }
            }
        }
        Ok(())
    }
}

impl ops::BitOr for Expr {
    type Output = Expr;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Expr::Or(mut s), Expr::Or(r)) => {
                s.extend(r.into_iter());
                Expr::Or(s)
            }
            (Expr::Or(mut s), r) | (r, Expr::Or(mut s)) => {
                s.push(r);
                Expr::Or(s)
            }
            (s, r) => Expr::Or(vec![s, r]),
        }
    }
}

impl ops::BitAnd for Expr {
    type Output = Expr;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Expr::And(mut s), Expr::And(r)) => {
                s.extend(r.into_iter());
                Expr::And(s)
            }
            (Expr::And(mut s), r) | (r, Expr::And(mut s)) => {
                s.push(r);
                Expr::And(s)
            }
            (s, r) => Expr::And(vec![s, r]),
        }
    }
}

impl ops::BitXor for Expr {
    type Output = Expr;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Expr::Xor(mut s), Expr::Xor(r)) => {
                s.extend(r.into_iter());
                Expr::Xor(s)
            }
            (Expr::Xor(mut s), r) | (r, Expr::Xor(mut s)) => {
                s.push(r);
                Expr::Xor(s)
            }
            (s, r) => Expr::Xor(vec![s, r]),
        }
    }
}

impl ops::Not for Expr {
    type Output = Expr;

    fn not(self) -> Self::Output {
        match self {
            Expr::Not(s) => *s,
            s => Expr::Not(Box::new(s)),
        }
    }
}

impl Expr {
    fn sub_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::And(_) | Expr::Xor(_) | Expr::Or(_) => write!(f, "({self})"),
            Expr::Not(_) => fmt::Display::fmt(&self, f),
            Expr::Value(v) => fmt::Display::fmt(&v, f),
        }
    }
    fn var<S: Into<String>>(s: S) -> Self {
        Expr::Value(Value::Var(s.into()))
    }
    const fn val(b: bool) -> Self {
        Expr::Value(Value::Val(b))
    }
    fn is_true(&self) -> bool {
        match self {
            Expr::Value(v) => v.is_true(),
            _ => false,
        }
    }
    fn is_false(&self) -> bool {
        match self {
            Expr::Value(v) => v.is_false(),
            _ => false,
        }
    }
    fn simplify(&mut self) {
        match self {
            Expr::And(exprs) => {
                for expr in exprs.iter_mut() {
                    expr.simplify();
                }
                let mut i = 0;
                while i < exprs.len() {
                    if matches!(exprs[i], Expr::And(_)) {
                        let Expr::And(sub_exprs) = exprs.remove(i) else {unreachable!()};
                        exprs.extend(sub_exprs);
                    } else {
                        i += 1;
                    }
                }
                // A&0=0 => set to false if any expr is false
                if exprs.iter().any(|x| x.is_false()) {
                    *self = Expr::val(false);
                } else {
                    // A&1=A => remove all 1's
                    exprs.retain(|x| !x.is_true());
                    // A&A=A => dedup
                    exprs.sort();
                    exprs.dedup();
                    match exprs.len() {
                        // no element => true
                        0 => *self = Expr::val(true),
                        // only 1 element => element
                        1 => *self = exprs.pop().unwrap(),
                        _ => {}
                    }
                }
            }
            Expr::Or(exprs) => {
                for expr in exprs.iter_mut() {
                    expr.simplify();
                }
                let mut i = 0;
                while i < exprs.len() {
                    if matches!(exprs[i], Expr::Or(_)) {
                        let Expr::Or(sub_exprs) = exprs.remove(i) else {unreachable!()};
                        exprs.extend(sub_exprs);
                    } else {
                        i += 1;
                    }
                }
                // A|1=1 => set to true if any expr is true
                if exprs.iter().any(|x| x.is_true()) {
                    *self = Expr::val(true);
                } else {
                    // A|0=A => remove all 0's
                    exprs.retain(|x| !x.is_false());
                    // A|A=A => dedup
                    exprs.sort();
                    exprs.dedup();
                    match exprs.len() {
                        // no element => false
                        0 => *self = Expr::val(false),
                        // only 1 element => element
                        1 => *self = exprs.pop().unwrap(),
                        _ => {}
                    }
                }
            }
            Expr::Not(expr) => {
                expr.simplify();
                if expr.is_true() {
                    *self = Expr::val(false)
                } else if expr.is_false() {
                    *self = Expr::val(true)
                }
            }
            Expr::Xor(exprs) => {
                for expr in exprs.iter_mut() {
                    expr.simplify();
                }
                let mut i = 0;
                while i < exprs.len() {
                    if matches!(exprs[i], Expr::Xor(_)) {
                        let Expr::Xor(sub_exprs) = exprs.remove(i) else {unreachable!()};
                        exprs.extend(sub_exprs);
                    } else {
                        i += 1;
                    }
                }
                // A^0=A => remove all 0's
                exprs.retain(|x| !x.is_false());
                // A^1=!A => invert expr
                let mut invert = false;
                exprs.retain(|x| {
                    if x.is_true() {
                        invert = !invert;
                        false
                    } else {
                        true
                    }
                });
                // A^A=0 => remove duplicates 2 by 2
                exprs.sort();
                let mut i = 0;
                while i < exprs.len() - 1 {
                    if exprs[i] == exprs[i + 1] {
                        exprs.remove(i);
                        exprs.remove(i);
                    } else {
                        i += 1;
                    }
                }
                match exprs.len() {
                    // no element => false
                    0 => *self = Expr::val(invert),
                    // only 1 element => element
                    1 => {
                        *self = if invert {
                            !exprs.pop().unwrap()
                        } else {
                            exprs.pop().unwrap()
                        }
                    }
                    _ => {
                        if invert {
                            // temporarily replace with dummy value
                            let s = mem::replace(self, Expr::val(false));
                            *self = Expr::Not(Box::new(s));
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // fn sexpr(&self) -> String {
    //     match self {
    //         Expr::And(exprs) => {
    //             let (first, rest) = exprs.split_first().unwrap();
    //             let mut s = format!("{}", first.sexpr());
    //             for next in rest {
    //                 s = format!("(and {} {s})", next.sexpr())
    //             }
    //             s
    //         }
    //         Expr::Or(exprs) => {
    //             let (first, rest) = exprs.split_first().unwrap();
    //             let mut s = format!("{}", first.sexpr());
    //             for next in rest {
    //                 s = format!("(or {} {s})", next.sexpr())
    //             }
    //             s
    //         }
    //         Expr::Not(expr) => {
    //             format!("(not {})", expr.sexpr())
    //         }
    //         Expr::Xor(exprs) => {
    //             let (first, rest) = exprs.split_first().unwrap();
    //             let mut s = format!("{}", first.sexpr());
    //             for next in rest {
    //                 s = format!("(xor {} {s})", next.sexpr())
    //             }
    //             s
    //         }
    //         Expr::Value(v) => match v {
    //             Value::Val(true) => format!("true"),
    //             Value::Val(false) => format!("false"),
    //             Value::Var(v) => format!("{v}"),
    //         },
    //     }
    // }
}

#[test]
fn test_simplify() {
    let mut v = Expr::var("a") | Expr::val(true);
    v.simplify();
    assert_eq!(v, Expr::val(true));
    let mut v = Expr::var("a") | Expr::val(false);
    v.simplify();
    assert_eq!(v, Expr::var("a"));
    let mut v = Expr::var("a") & Expr::val(true);
    v.simplify();
    assert_eq!(v, Expr::var("a"));
    let mut v = Expr::var("a") & Expr::val(false);
    v.simplify();
    assert_eq!(v, Expr::val(false));
    let mut v = Expr::var("a") ^ Expr::val(true);
    v.simplify();
    assert_eq!(v, !Expr::var("a"));
    let mut v = Expr::var("a") ^ Expr::val(false);
    v.simplify();
    assert_eq!(v, Expr::var("a"));
    let mut v = !Expr::val(true);
    v.simplify();
    assert_eq!(v, Expr::val(false));
    let mut v = !Expr::val(false);
    v.simplify();
    assert_eq!(v, Expr::val(true));
    let mut v = Expr::And(vec![
        Expr::var("a"),
        Expr::And(vec![
            Expr::var("b"),
            Expr::And(vec![Expr::var("c"), Expr::var("d")]),
        ]),
    ]);
    v.simplify();
    assert_eq!(
        v,
        Expr::And(vec![
            Expr::var("a"),
            Expr::var("b"),
            Expr::var("c"),
            Expr::var("d")
        ])
    );
    let mut v = Expr::Or(vec![
        Expr::var("a"),
        Expr::Or(vec![
            Expr::var("b"),
            Expr::Or(vec![Expr::var("c"), Expr::var("d")]),
        ]),
    ]);
    v.simplify();
    assert_eq!(
        v,
        Expr::Or(vec![
            Expr::var("a"),
            Expr::var("b"),
            Expr::var("c"),
            Expr::var("d")
        ])
    );
    let mut v = Expr::Xor(vec![
        Expr::var("a"),
        Expr::Xor(vec![
            Expr::var("b"),
            Expr::Xor(vec![Expr::var("c"), Expr::var("d")]),
        ]),
    ]);
    v.simplify();
    assert_eq!(
        v,
        Expr::Xor(vec![
            Expr::var("a"),
            Expr::var("b"),
            Expr::var("c"),
            Expr::var("d")
        ])
    );
}

#[derive(Debug, Clone)]
struct Expr32([Expr; 32]);

impl Expr32 {
    fn var<S: Display>(s: S) -> Self {
        Expr32::gen(|i| Expr::var(format!("{s}_{i}")))
    }
    fn val(v: u32) -> Self {
        Expr32::gen(|i| Expr::val((v >> i) & 1 == 1))
    }
    fn gen<F: Fn(usize) -> Expr>(op: F) -> Self {
        Expr32([
            op(0),
            op(1),
            op(2),
            op(3),
            op(4),
            op(5),
            op(6),
            op(7),
            op(8),
            op(9),
            op(10),
            op(11),
            op(12),
            op(13),
            op(14),
            op(15),
            op(16),
            op(17),
            op(18),
            op(19),
            op(20),
            op(21),
            op(22),
            op(23),
            op(24),
            op(25),
            op(26),
            op(27),
            op(28),
            op(29),
            op(30),
            op(31),
        ])
    }
    fn apply_all_bits_with<F: Fn(Expr, Expr) -> Expr>(self, rhs: Self, op: F) -> Self {
        let mut s = self.0.into_iter();
        let mut r = rhs.0.into_iter();
        Expr32(
            iter::repeat_with(|| op(s.next().unwrap(), r.next().unwrap()))
                .take(32)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        )
    }
    fn apply_all_bits<F: Fn(Expr) -> Expr>(self, op: F) -> Self {
        let mut s = self.0.into_iter();
        Expr32(
            iter::repeat_with(|| op(s.next().unwrap()))
                .take(32)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        )
    }
    fn simplify(&mut self) {
        for bit in &mut self.0 {
            bit.simplify();
        }
    }

    fn shift_right(self, n: u32) -> Self {
        let s = self.0.into_iter();
        Expr32(
            s.skip(n as usize)
                .chain(iter::repeat_with(|| Expr::val(false)))
                .take(32)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        )
    }
    fn expand_bit(bit: Expr) -> Self {
        Expr32(
            iter::repeat(bit)
                .take(32)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        )
    }
}

impl ops::BitAnd for Expr32 {
    type Output = Expr32;

    fn bitand(self, rhs: Self) -> Self::Output {
        self.apply_all_bits_with(rhs, |a, b| a & b)
    }
}

impl ops::BitOr for Expr32 {
    type Output = Expr32;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.apply_all_bits_with(rhs, |a, b| a | b)
    }
}

impl ops::BitXor for Expr32 {
    type Output = Expr32;

    fn bitxor(self, rhs: Self) -> Self::Output {
        self.apply_all_bits_with(rhs, |a, b| a ^ b)
    }
}

impl ops::Not for Expr32 {
    type Output = Expr32;

    fn not(self) -> Self::Output {
        self.apply_all_bits(|a| !a)
    }
}

impl fmt::Display for Expr32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct Shim<T>(T);
        impl<T: fmt::Display> fmt::Debug for Shim<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }
        let mut l = f.debug_list();
        for x in &self.0 {
            l.entry(&Shim(x));
        }
        l.finish()
    }
}

const N: usize = 624;
const M: usize = 397;

const LOWER_MASK: u32 = 0x7FFFFFFF;
const UPPER_MASK: u32 = 0x80000000;

const A: u32 = 0x9908B0DF;

const SEQ: [&[u8; 32]; 80] = [
    b"\x61\x17\x47\x72\xe2\xe9\x0b\xad\x86\x5c\x09\x67\x30\x61\x97\x15\x0e\x88\x68\xa2\xd3\x42\xd8\xc0\x4f\x39\xfd\xcf\xe8\xa6\xaa\x24",
    b"\x17\x69\x1f\xeb\x21\xc1\x0a\xc3\x16\xfa\x91\x8a\x6f\xf0\x16\x59\xea\x4c\x23\x41\x23\xc9\x66\x3a\x60\xb8\xfd\x02\xad\xd0\xf1\x4f",
    b"\xab\x70\x2c\x8e\xb0\x4c\xa7\xfc\x09\xa5\x1d\x03\xd7\x4c\xfb\xd6\x04\xa7\xb6\x6f\x70\xb5\xb9\xc7\x61\x2e\xbf\xee\x8d\xd5\x9b\x78",
    b"\xfa\x0f\xc7\xf1\x06\x38\x89\x5f\xd0\xfc\x72\x4b\x57\x3c\x1d\x6e\x81\x71\x03\x1c\x4a\xa5\xa6\xcb\x9e\x9c\x06\x63\x25\x54\x6e\xf4",
    b"\xca\x9e\x1a\x4a\xdc\x46\x37\x6d\xec\x29\x8e\xce\x73\x9d\x7a\xa0\x3e\xf3\xb9\xfa\xf4\xbc\xb5\xa8\x63\xdc\x7f\xb0\x5d\x15\x40\x55",
    b"\xe1\xfb\xd5\x8b\x37\xaf\xcf\x6a\x8c\x79\xc7\x22\x5c\x73\xc5\x07\x40\x21\xb1\x8d\x38\xd4\x98\x94\xdc\xc3\xe8\x11\x07\x81\x21\xf3",
    b"\x33\x7e\x88\x08\x47\x4a\x89\x41\xb1\xe0\xad\x26\x4a\xae\xa0\xcb\x6c\xe9\x87\xdb\x74\xa9\x04\xf0\xac\x95\x48\x57\xd1\x41\xfe\x2e",
    b"\xab\x28\x4e\x5e\x2d\xb8\x40\xe0\xd0\xab\x6f\x2c\xc5\xda\x23\xdd\x89\xc2\x74\x4f\x2f\x70\x2a\x65\x04\xee\xfa\x50\x04\xdb\xcb\x25",
    b"\x8a\x01\x12\x12\xac\x55\x8a\xb3\x57\x92\x2c\x58\x88\x4c\x78\xd4\x0e\x39\x8b\x39\x5c\xbf\xbc\x6d\xde\x1b\x8c\xd2\x04\xd3\x8b\xc6",
    b"\x70\x5c\xde\x20\x21\x53\xe7\xab\x79\xb9\xe9\x3c\x94\xce\x2b\xfb\x2a\xe0\x4d\xd9\xbc\x1d\x9f\xe9\x20\x30\xfb\x9d\x49\x02\xfa\xe4",
    b"\x03\xdf\x87\x2b\xa5\x0d\x34\xe0\x67\xe0\x3b\x06\x58\x69\x9d\xc4\x24\x00\xb0\x40\x21\xa4\xfa\x4d\x14\xcb\x1b\x7e\x5e\xbf\x65\x36",
    b"\x7b\xad\x07\x2c\x19\x51\x45\xab\x9a\x22\x9d\x2b\x8c\xff\x39\x7e\x47\xd2\x6c\x1d\x3f\x6b\x7c\xd2\x58\xa5\x15\xbb\xfd\x93\xb5\x9f",
    b"\x38\xfd\x5c\x1b\x70\xb9\x15\x1f\x1e\x55\x8a\xdf\x10\x37\x0b\x2d\x79\xbc\x02\xae\xc5\xce\xf0\x98\xa7\x57\x05\x19\x5f\x10\x8d\x34",
    b"\x02\x78\x91\xf3\xd8\xa1\xd5\x0b\x71\x39\xbf\x92\xce\x55\x19\xa9\xf5\xb7\xfb\x83\x1c\x13\x6c\x91\x81\x0f\x06\x5c\x6a\x04\x2c\x7a",
    b"\xae\x4a\x35\x39\xc9\x9d\xe7\x97\x3b\x44\xda\xf6\x68\xbe\x59\xfe\x92\x13\x2c\x54\xae\x3a\xbd\xa6\x9d\x8d\xe2\xed\xad\x49\x1d\xb8",
    b"\x82\x13\xba\x37\x8e\xe8\xea\xa0\x50\x57\x3b\xfb\xd9\x21\xc4\x5b\xd8\xb3\x02\x2d\xf2\x4e\x37\x4a\xae\xbd\x4d\x36\x60\x7e\x1a\x90",
    b"\xbf\x4a\x19\x2b\xac\x6a\xfd\x56\x7b\x86\x1c\xd2\x39\xf4\x0c\x07\xe7\x4c\x56\xc8\xad\xd2\x9a\x1d\xb7\x91\x5e\x04\x11\x66\xf1\xdb",
    b"\x89\xe5\xca\x4f\xcf\xd5\x1b\x55\xe7\xa2\xec\x62\x11\x60\xd3\x0b\x47\xfd\x52\x59\x46\x29\x7b\x5d\x06\xfb\x3e\xdd\x0c\x95\x0a\x14",
    b"\x99\x1f\x2f\x60\xeb\x1d\x91\x88\x07\x5f\xfd\xbb\xf5\xdc\x1a\x41\xd4\x34\x76\x76\xc7\x3c\x77\xed\xbe\x85\xb0\x73\xe1\x3c\x0e\xff",
    b"\x12\x48\xf5\x42\xec\x61\x7a\xca\x56\x10\x65\xaf\x1b\xfc\x64\x90\x68\xf8\xb9\xb2\xec\xa5\x2a\x0f\x0e\x47\xfc\xbb\xa7\xe9\x2c\x1e",
    b"\xc4\x14\x47\xc5\x2e\xfa\xa4\xad\x70\x5c\x7f\xb4\x57\x27\xf6\x98\x64\x0e\x46\xcc\xea\xb3\x4d\x69\xcc\x18\xf5\x2a\xcc\xf5\x0f\x67",
    b"\x4b\x64\x72\xe9\xfb\x7b\xd3\x77\xa3\x95\x58\x09\x4d\x39\x9a\x92\x53\xae\x81\x70\xed\x6c\x01\x13\xbe\x64\xbd\xfa\xd9\x56\x10\x98",
    b"\xef\x73\xc0\x77\x4b\xf5\x4a\x42\x0b\x94\xf3\x86\x66\xc8\x19\x53\x85\x57\x2c\x51\x7c\xdb\xd4\xc4\xcc\x95\x3f\xd4\x12\x15\x52\xf7",
    b"\x98\x5e\xa7\xc0\x36\x7e\x20\xe4\x9a\xd0\x56\xb6\x4b\x36\x08\xa2\x05\x12\xfb\xf8\x5d\x0d\x6d\xa2\x63\xcb\xfc\x12\xe5\xbe\x11\x08",
    b"\x4a\x98\x52\x61\x40\x36\xd7\x2d\xe8\x16\xe6\x95\x76\xbc\x2b\x9c\x92\x60\xef\x69\x81\xfe\x79\xf8\xef\xe5\xed\x14\x33\x82\xd4\xed",
    b"\x1a\x07\x4a\x08\x81\xf0\xed\xe6\x62\x28\x75\x32\xa1\x3b\x37\xee\xef\x6e\x15\xc7\x68\x42\x9b\x82\x24\x85\x83\xd0\x54\xd4\xa1\x53",
    b"\x7d\x30\x14\x22\x30\x2c\x69\x81\x11\x5d\xcb\x5c\xbc\xaf\xcb\x7f\xf1\xc5\x09\xbb\x38\xb0\x5f\x55\xe8\x69\x6e\xc7\xba\x70\xc8\x64",
    b"\xdd\xe3\xa7\xc9\x57\x3f\x61\x05\x7d\x54\xea\x99\xc1\x0b\xb7\x1a\x45\xab\xc7\x2d\xeb\xa2\xf4\xf1\xed\xe9\x77\xe1\xe2\xb7\x57\xe4",
    b"\x20\x86\x49\x90\x59\x21\x65\x9e\x17\xb5\x1d\xbd\x65\xbe\xfc\x95\x26\x52\xad\x7c\x3d\xc2\x4a\x54\x0c\x18\x07\xbc\x8f\x8f\xbb\x5e",
    b"\x02\x2b\xc7\x9d\x44\xb7\x1c\xda\xf1\xd1\xec\x6e\x9d\xf6\x50\xad\xe3\x62\xda\x95\x70\x87\x22\xda\xa9\x7c\xf1\x50\x28\x58\x3d\xf2",
    b"\xf7\xcd\x09\x66\x2d\xfb\x73\x59\x79\x25\x52\x9c\x3d\x59\xd3\x18\x0e\x58\x3e\xda\x73\xab\x85\xf3\xe1\x5a\x26\xec\x72\xa2\x95\x08",
    b"\xbd\x0c\xbc\x3d\xec\x2d\xef\xe9\x37\x7b\xb5\xc1\x9b\x84\xc2\x68\xb8\x95\x5e\x4d\xcb\xfd\x61\x63\xec\x68\x09\x9c\x43\x8c\xfc\xc7",
    b"\x9f\x56\xaf\x18\xd6\xf8\x4f\xea\x11\xc5\xcb\x68\x66\xd6\x25\x40\x15\xf5\xd6\xcf\x44\x1c\x96\x50\x8b\x45\xc1\xdf\x2e\x51\x53\x78",
    b"\xc2\x56\xce\xcd\x98\xc0\xe1\xce\xf3\x21\xc9\xcc\xc0\x60\xd8\x80\x39\x54\x05\x69\x77\x76\xe4\xf4\x7c\xdf\x96\xa2\x46\x11\xa1\x73",
    b"\x36\x70\x05\x7d\x8e\x0a\xa7\x06\xd0\xc5\x7e\x5e\x2b\x06\xce\xc5\x64\xf9\xab\x64\x8a\xab\x29\xc7\xd7\xa2\xcc\x04\x3d\x25\x2a\x75",
    b"\x34\xff\x10\x5b\xd2\x89\xf6\x73\x86\x44\x70\x5c\x4d\xb0\x31\x01\x4f\x35\x5f\x2f\x65\xd7\x15\x4a\xbe\xe6\x48\x6b\x35\xbc\xcb\x08",
    b"\x4d\x6b\x92\xc1\x6f\xbf\x57\x31\xd0\x46\x25\x88\x5c\x05\xc4\x3d\x6c\xd1\xc4\xf2\x1c\xdc\xec\xaa\xdb\x9c\xe8\x07\xe3\x3a\xf4\xa5",
    b"\xa1\x10\x33\xbd\x74\x38\xc8\x29\xcb\x66\xcb\x17\xf2\xb7\xe1\xf5\xd7\xf0\xf1\xfa\x4f\x1e\x9f\x38\x4e\x6b\x63\xe2\x6f\x9b\x9d\xab",
    b"\x23\x50\x95\x8f\x9c\x0b\x8b\x1e\x1d\xb8\xd1\xc9\x85\xce\xab\x50\x76\x68\x70\x77\x6a\xcb\xc1\xd6\xee\xdd\x6f\x0d\x1b\x76\xd6\x29",
    b"\x29\x5a\x09\x2f\xa5\x42\x72\xba\x56\x07\xa7\xcd\x3c\xf3\x83\x5d\x6b\xb2\xc4\xcf\x44\x99\x78\x7c\xa7\x92\xc8\x07\xbc\xc2\x1a\x7c",
    b"\x37\x42\x5b\xfb\x53\x8e\x07\x65\x58\xdd\x55\x47\x01\x36\xd5\xbf\x79\xad\x70\x1f\x7b\x93\x9f\x98\x0d\xf4\xc1\x03\x70\x42\xe6\x7f",
    b"\x21\x5a\xee\x34\xe9\x54\xd3\xc4\x11\xd1\x39\x87\xac\x58\x23\x1f\x1c\x0e\x1c\xd2\x91\xce\x0f\x2c\xd1\x23\x3d\xfd\x20\xd4\x23\x75",
    b"\xe5\x52\x9d\x75\x90\xd0\x17\xf4\xf3\xbc\xc6\xb4\xc9\x45\x8a\x36\x5c\x07\xef\x17\x21\x7c\xe8\xe3\x06\xd2\xcc\xab\x09\xb6\x9b\xc7",
    b"\x41\xbc\x1a\xc3\xd6\xe8\xa4\xb2\x82\x96\x67\x18\x68\x64\xa2\xbf\xd2\xaf\x5a\x5a\xad\xec\xc6\xaf\x7a\xc1\x35\x2f\xb1\x95\xca\x4c",
    b"\x8a\xa6\xd5\x54\x2b\x13\xb2\xb5\x3c\xb1\x96\x49\x2d\x70\x3b\x4c\x29\xe6\x4d\xeb\x74\xc0\x43\xfb\x44\xb7\x54\x4b\x69\xe4\x84\x85",
    b"\xe9\x76\xe1\xde\x0b\x81\x51\x56\xdf\xf6\xe9\x6e\xa9\x37\x75\xfd\x92\x6f\x2c\x49\x06\xd0\xef\x8d\x9a\x37\xb9\x32\xeb\x72\xbf\x69",
    b"\xf8\xce\xeb\xf3\xcb\x9f\x2f\xe1\xb7\xf5\xb1\x65\x4a\xd4\x93\x3d\xb5\xfb\x58\x40\x0b\xd3\x4c\xec\x15\xd2\x01\xdc\xc6\x1c\x4b\xd7",
    b"\x33\x2b\x11\xdc\x95\x2b\xa9\xb6\x78\xa1\xc8\x20\x94\x47\x98\xb2\x16\x61\xa1\x18\xd3\x68\xe6\xed\x3b\xb4\xaa\xd7\xfa\xb3\xd3\x33",
    b"\x43\xd8\xe3\xc9\x13\xf7\x25\x7d\x14\xf8\xc3\xfb\x8d\x21\x0f\x0b\x8b\x90\x0c\x64\x19\x6f\xf9\xe6\x97\x38\xf2\xbd\xda\x85\x1c\x73",
    b"\x11\x6b\x4e\xea\x40\x8d\xe7\x21\x2f\x85\x65\xbc\xbb\x42\x01\x19\x34\x35\xd7\x8d\xe8\xa2\x82\x68\xa4\x9c\x85\xf4\xa4\xe9\xe2\xf6",
    b"\x82\xb0\xb1\xf4\xf9\x46\x2c\xf6\x64\xf6\x07\x51\x80\xe2\x9b\xac\x7e\x98\x33\x74\x5b\x1b\x0e\xfc\x06\xa2\x28\xb6\x22\xfc\x3b\x53",
    b"\x00\x21\x04\x0f\x20\x8b\x7a\x49\x3d\x14\xda\x84\x95\x7b\xc8\x0c\xe2\x4c\x6f\xc9\xc7\xa3\x00\xec\x74\xb0\x1e\x89\x33\xb2\xcc\x35",
    b"\x9d\xce\xe5\xa1\x82\x21\x8c\xce\xd1\xec\x5a\xf3\x82\x42\x40\x44\xf2\x2a\x9c\x06\xaf\x04\xd9\x05\x99\x55\xdc\xec\x08\x19\xf9\xd9",
    b"\x05\x7c\xe4\xd3\x4d\xfb\xbd\xd8\x17\x6a\xea\x46\x19\x7f\x78\xef\x22\x99\x97\x9c\x96\xec\xdb\x60\xbe\x00\xa9\x16\x81\x4c\xdd\x8c",
    b"\x46\x52\xd9\xed\x3f\x6d\x2f\xc1\xe9\x69\x11\x50\xed\x05\xb6\xa1\x27\xfc\xc1\x88\xb4\xc8\x36\x06\x0c\xdf\xb7\xb8\xb1\x19\xa2\xe1",
    b"\x18\xcb\xf0\xa5\xb5\xf4\xbc\xf7\x85\x9e\x37\xf2\x6c\x8f\xdb\xdd\x49\x7b\x5b\x74\x9a\x84\xf1\xf3\xce\xa6\x1a\xee\x0b\x90\x0a\x98",
    b"\x68\x24\x92\x9f\xc7\x03\xb6\x24\xe8\x98\x82\x99\x7e\x60\xfe\x6e\x9b\x9c\x4f\x56\xec\xf7\x3b\x70\xd0\x32\x58\xb8\xf8\xe5\xe1\xf8",
    b"\x11\x70\x86\x0f\xd8\x52\x83\x8f\x50\xa6\x8b\xf6\xd9\x84\x88\x3c\x4d\x24\x24\xb5\xa1\x3a\x09\xf7\xef\xed\xc5\x23\x82\x5e\x18\xd0",
    b"\x88\x96\xbf\x46\x05\x95\xbc\xd2\x5a\x28\x0d\xef\xd1\x5d\xb3\x4d\x53\xb3\xd8\xb3\x39\x05\x40\xe7\x8c\x1f\x0f\xaa\x7f\x21\xfb\xb2",
    b"\x1a\x7e\x01\x2b\x7a\x12\xc6\x0a\xd2\x74\x57\x12\x58\x44\xd2\x9c\xf3\xbd\x73\xf8\xe3\xa2\xc9\xf1\x18\x84\x5f\x01\x66\x5f\x1c\xec",
    b"\x0e\x89\xa0\xb1\x93\xc4\xc2\x7c\xf8\xfd\xf7\xf2\x79\xab\x88\x95\x83\xce\x14\x26\xd8\x6e\xc6\xff\x4e\xf4\xf7\xff\x78\x6c\xce\x90",
    b"\x9f\x37\xa8\xcb\x52\x86\x62\xb2\xf7\x06\xc1\xe3\x3b\x85\xd7\x6b\x16\xde\x03\x05\x38\x67\xd6\x7b\x8c\xc6\x69\xc7\xe5\xfe\x00\x4c",
    b"\x77\xb5\x4f\x03\x17\xbc\xdc\xd1\x1c\x27\xe4\x53\xa4\x9f\xbb\xf5\xd2\x12\xec\x23\x60\x42\x70\x77\xf6\x28\x7a\x2f\x22\x9e\xe5\x31",
    b"\x76\x80\x95\xf9\x92\xfd\x89\xa2\x08\xb4\x10\x5c\xaf\xc3\xe2\x8e\xf7\x8c\x59\xc1\xaa\x0f\x4b\x1b\x41\x7c\x9e\x0d\xb7\x4a\x68\x28",
    b"\xa3\x82\x0f\x9a\x90\x7c\x7f\x85\xe2\x2f\x01\xf5\xfe\x1c\x57\xe1\xe4\x96\x9b\xe4\x69\x72\x96\x2c\x5b\x75\xf2\x14\x4d\xce\xd7\x65",
    b"\x99\xc1\x6f\x31\x5d\xbe\x13\xd9\x66\x19\xd9\xeb\xdf\xbe\x16\x87\xb1\x0f\x5d\x4c\xa6\x57\xaf\x6b\x8b\x7c\x6e\xcd\xca\x64\x3f\x0c",
    b"\x1f\xb9\x7a\xf6\xfb\x12\x44\xa0\xba\x65\xf3\x24\x19\x9c\x55\xa0\xb2\x7b\x31\x35\xfd\x91\xe7\x85\xa8\x4e\x92\xcd\x72\xb3\x09\x5d",
    b"\xb1\x92\xa2\x39\x31\xaf\x97\xc1\x4f\x57\xf2\xf4\x9d\x40\xc3\x78\x2d\x60\x89\x09\x29\x58\xf7\xc7\x4c\x38\xea\x3a\xa6\x13\x6c\x18",
    b"\xd3\x52\xbc\xd3\xbe\xff\xbb\xc4\x68\xae\x75\x2a\xbc\xb4\xe4\xd3\x0d\x32\xa6\x95\x3d\x24\x45\xeb\x1e\xd8\xae\x0a\xe9\x32\x2b\xad",
    b"\x14\xf6\x4f\xc6\x53\x27\xc7\x91\xc0\x61\x50\x48\x0a\x71\xa6\xd0\xf1\x52\x56\xd7\x56\x09\xbc\x19\xb5\x6b\x22\x9b\xa5\x12\x68\x63",
    b"\x75\x0f\x5e\x01\xcd\x3b\xbe\xcd\x01\xa9\xad\xc3\x2b\x7a\x6c\x94\x6b\x1f\x6b\xfd\xb0\xac\x58\xf9\xda\x5e\xf3\xa1\x4c\x46\x00\x0e",
    b"\x80\xf3\x92\x00\x93\xfd\x5d\x2a\xd6\x8d\x41\x3a\x7f\x70\x08\xeb\xac\xaf\x34\x0d\xb8\x22\xdd\xaa\xaf\xbb\xc8\xa6\x3b\xf9\x2d\xff",
    b"\x30\x45\x89\x42\x0f\x49\x21\x7d\x0a\x09\x18\xc5\x9c\x37\xdd\x78\x73\x70\x7f\xde\x53\xcd\xa9\x8a\xd9\xcc\x2c\xb3\x5b\xd6\xd8\x7b",
    b"\x77\x9f\x41\x58\x92\x10\x7f\xea\x7b\x01\x15\x83\x3a\x79\x73\xf5\xe7\x24\x2f\xb6\x5f\x2a\x65\x3c\xf3\x5c\x35\x39\xb7\xfb\xd2\x7e",
    b"\xce\xbe\x54\x9d\x49\xce\xa3\x5a\x49\x64\xa1\x40\x6e\xff\xe1\x1d\x26\x99\xbb\xd3\x03\xb1\xa2\x06\x97\x40\xd0\x45\xfb\x57\x03\xce",
    b"\x0e\x03\x23\x22\x7f\x04\x95\x33\x89\xb2\xd2\x02\x5b\x12\x91\x8e\x36\xc6\xb1\x7a\x23\x3c\x63\x5a\x6e\x5b\xa3\x16\x83\x9a\x16\x68",
    b"\x3a\x1e\xb5\x8e\x03\xc1\x1d\xd5\x21\x5c\xbe\xaa\xae\xda\x4d\x68\x38\x4c\xf1\xc6\x65\x9b\x90\xa3\x47\x60\x98\x5e\x40\x61\x88\xec",
    b"\x0c\x53\x07\xa6\x1f\x5f\x02\x55\xe0\x1e\x6a\xd5\xde\x09\x78\xd7\x9c\xa6\xed\x0d\x9c\x66\x44\xb5\x52\x3f\x0f\xff\x99\xde\xe1\xd5",
    b"\x31\x64\xe0\x99\x7d\x92\x92\x9a\x48\x53\x67\x08\x35\xf2\x79\x61\x99\xef\xc8\x15\x9d\xa2\xf6\x48\x41\xaf\x28\x6a\x39\x53\x78\x86",
    b"\xf9\x0c\xf0\x42\x32\x4c\x64\x1c\x9a\xb5\xa8\x48\xf1\x5b\x51\xb3\x1c\x0d\xe3\x40\xec\xeb\x09\x6e\xee\xfa\x69\xc9\xf8\x6b\xc1\x28",
];

struct SeqGen(usize);
impl Iterator for SeqGen {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        let chunk = self.0 / 256;
        let byte = (self.0 % 256) / 8;
        let bit = self.0 % 8;
        self.0 += 1;
        SEQ.get(chunk)
            .and_then(|chunk| chunk.get(byte))
            .map(|&byte| (byte >> (7 - bit)) & 1 == 1)
    }
}

struct MT19937Breaker {
    mt: [Expr32; N],
    i: usize,
    system: Vec<(Expr, bool)>,
}
impl MT19937Breaker {
    fn new() -> Self {
        Self {
            mt: (0..N)
                .map(|n| Expr32::var(format_args!("mt_{n}")))
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
            i: 0,
            system: Vec::new(),
        }
    }
    fn twist(&mut self) {
        for i in 0..N {
            let x = (self.mt[i].clone() & Expr32::val(UPPER_MASK))
                | (self.mt[(i + 1) % N].clone() & Expr32::val(LOWER_MASK));
            let mut x_a = x.clone().shift_right(1);
            x_a = x_a ^ (Expr32::expand_bit(x.0[0].clone()) & Expr32::val(A));
            self.mt[i] = self.mt[(i + M) % N].clone() ^ x_a;
            self.mt[i].simplify();
        }
    }
    fn feedbit(&mut self, b: bool) {
        if self.i >= N {
            self.twist();
            self.i = 0;
        }

        let mut y = self.mt[self.i].0[31].clone()
            ^ self.mt[self.i].0[24].clone()
            ^ self.mt[self.i].0[16].clone()
            ^ self.mt[self.i].0[27].clone();
        self.i += 1;
        y.simplify();
        self.system.push((y, b));
    }
}

#[test]
fn test_impl() {
    const U: u32 = 11;
    const D: u32 = 0xFFFFFFFF;
    const S: u32 = 7;
    const B: u32 = 0x9D2C5680;
    const T: u32 = 15;
    const C: u32 = 0xEFC60000;
    const L: u32 = 18;

    struct MT19937 {
        mt: [u32; N],
        i: usize,
    }

    impl MT19937 {
        fn from_state(i: usize, state: [u32; N]) -> Self {
            Self { mt: state, i }
        }
        fn twist(&mut self) {
            for i in 0..N {
                let x = (self.mt[i] & UPPER_MASK) | (self.mt[(i + 1) % N] & LOWER_MASK);
                let mut x_a = x >> 1;
                x_a = x_a ^ (if x & 1 == 1 { 0xffffffff } else { 0 } & A);
                self.mt[i] = self.mt[(i + M) % N] ^ x_a
            }
        }
        fn getrandbits(&mut self, n: usize) -> u32 {
            if self.i >= N {
                self.twist();
                self.i = 0;
            }

            let mut y = self.mt[self.i];
            y = y ^ ((y >> U) & D);
            y = y ^ ((y << S) & B);
            y = y ^ ((y << T) & C);
            y = y ^ (y >> L);

            self.i += 1;
            return y >> (32 - n);
        }
    }

    const ZERO_STATE: [u32; N] = [
        2147483648, 766982754, 497961170, 3952298588, 2331775348, 1811986599, 3100132149,
        3188119873, 3937547222, 215718963, 3315684082, 2978012849, 2428261856, 1298227695,
        1704729580, 54668373, 3285201915, 3285178464, 1552935063, 988471319, 3135387943,
        1691402966, 2757551880, 416056905, 907387413, 1072924981, 33903495, 2168419592, 2429050353,
        831159753, 430343641, 3315943586, 1761671042, 864453023, 334804929, 1627478028, 2596811275,
        3468733638, 3994375553, 1457139722, 3139722021, 1334790738, 2656639915, 3535811098,
        1464315470, 2397423927, 885719490, 1140895889, 3284299483, 2854516462, 2734973817,
        147484763, 792049954, 114360641, 3345458839, 1159898878, 1410498733, 2242989638, 453922141,
        1344019764, 413870456, 3089405849, 1494382840, 470157779, 4266372830, 2831181573,
        1361928602, 1589253513, 1381373062, 753045124, 987032420, 781978839, 2953638767,
        3258570111, 3006718191, 1675218601, 1854232715, 3655829819, 1731242722, 2192104666,
        1736665161, 740150002, 1195833394, 1610203160, 159492766, 4041488705, 3128952632,
        2867295744, 3272632449, 886824304, 1791482600, 221114776, 3867175393, 4020804062,
        1077871826, 1298953503, 996366221, 4149754679, 2483052703, 2615558283, 274318093,
        1716359450, 4099129961, 1026774175, 288240973, 1459347562, 2365566296, 3690105224,
        3065780221, 2050634722, 2652606621, 3185241207, 3026457375, 3456165734, 1880121515,
        3398461093, 1795638629, 2379692076, 608668379, 1261955525, 84456522, 1913485156, 106878280,
        757183891, 2913957588, 160418091, 2025664758, 141497907, 1657818026, 3053760160, 672193054,
        4157546743, 223046484, 1623470498, 1201972930, 675008814, 684162366, 1738776330,
        3025656654, 159760723, 1908867305, 3933381342, 2545706671, 467196949, 1427819885,
        842150314, 4032903454, 2140851898, 3269883445, 975813755, 4177392955, 1556690684,
        2535611513, 462962732, 67591358, 1729610528, 2025206740, 3153739740, 3255032049,
        4186226368, 1070144624, 3107867195, 1621006038, 63742485, 835629717, 3189842019,
        3950227584, 3184714559, 841836938, 1685394870, 657939920, 766156242, 1412314179,
        1048281639, 4037161120, 2044490307, 1923947830, 3900790422, 907554295, 276417304,
        860658646, 3574201134, 3508771399, 2110232300, 1636296241, 1405006077, 1093408401,
        3243057343, 1519791182, 1994660136, 3829840937, 2644974199, 957955566, 3487641161,
        1646922510, 1907939989, 3836029453, 3429168778, 201307778, 72550089, 2464394982,
        1695794191, 3344785682, 996786130, 3589457196, 1241754792, 1291082245, 4224603667,
        1194379475, 2693491244, 881186965, 2705535111, 445306946, 440274268, 1980827733,
        2482488861, 3205215943, 2119332222, 2928713046, 1418736938, 652581136, 2474070665,
        2208621536, 4171251876, 2303664214, 443762656, 2981912989, 2199228311, 2652261633,
        3166738494, 3443009210, 3498764432, 424010848, 4065487566, 2262993542, 1756076712,
        1477098233, 2742171915, 306185806, 3610666541, 923091830, 1034267993, 2336668648,
        1880719718, 676878038, 3788797208, 3763351494, 3985428106, 1101865631, 1130501258,
        3672967388, 3432003530, 4124438011, 1660392285, 4025484827, 2108074566, 3815409682,
        42955331, 3248965569, 1643835718, 1246665668, 1071162194, 3814069229, 115491158, 985096811,
        3311029186, 2990827378, 3101633320, 1648574497, 1470117052, 174145027, 2019894819,
        2035501481, 459104123, 3507464599, 2093352659, 3369174406, 618767835, 4009895756,
        935587447, 3956987426, 33753995, 307782427, 2473424805, 1440371818, 2382619594, 2138695812,
        3164510238, 1318650933, 2910086616, 3886677510, 566832801, 3718063320, 1559818704,
        183047272, 1142362855, 26306548, 645536402, 3875596208, 2272778168, 3512733409, 1897046338,
        38248886, 2570759766, 1806313150, 860304898, 2433450338, 4124013408, 1216634590,
        1275388896, 1169566669, 652504502, 761221427, 1448403764, 3129135949, 2513214949,
        1269533687, 2413509541, 1226750363, 2450740925, 4094137910, 945759293, 3636927736,
        3178020081, 2509964157, 3878869300, 1848504895, 2018369720, 1579755740, 1023627943,
        924838836, 2653160914, 1812804174, 1521323076, 4012390528, 1338763317, 2608655937,
        16022784, 1672945066, 2177189646, 2944458483, 2213810972, 1369873847, 1224017670,
        130901785, 3595066712, 2259115284, 3316038259, 455873927, 2917250465, 3599550610,
        1502173758, 684943436, 3079863840, 3144992244, 942855823, 1771140188, 2118780653,
        3411494225, 2711180217, 4239611184, 1371891067, 3398566397, 3105518599, 1310665701,
        3345178451, 2959821156, 242241789, 2148966880, 3192740583, 404401893, 3605380577,
        1446464038, 3920522056, 2577523013, 1079274576, 286634372, 1752710796, 2351075979,
        981312309, 3410516352, 3468455736, 1938779182, 1592494371, 1533303080, 88045436, 438252489,
        1220512168, 3487004938, 3724852871, 1073434882, 3728218947, 2977555283, 4105408406,
        3553772656, 1462006821, 3917158017, 119003006, 3470530198, 3439192457, 2829375771,
        3555715155, 32324691, 588735808, 1459221702, 803072782, 2699519868, 1530797005, 79738580,
        671990400, 4289511388, 3207115447, 2584684068, 832698998, 760958416, 1217440464,
        2517898131, 2418819938, 3629956222, 3445024962, 206619378, 365007395, 522114139,
        1707954431, 540423623, 1786750801, 369253262, 4239016754, 147889201, 1637777773, 236798285,
        2806120188, 586972608, 2201782716, 1323327827, 819485723, 406078680, 3407345698,
        1537169369, 1821691865, 527271655, 3751827102, 1465426495, 3321682429, 2179672664,
        401355478, 1068871880, 24609462, 1403522408, 2311580015, 1532058170, 3877815340,
        1768430711, 1619755157, 2832904331, 475102697, 354987331, 3295386430, 2816873951,
        1039415736, 363972779, 1499307670, 2895506264, 3746345349, 2678027234, 3251899088,
        955392878, 2329157295, 1343358773, 309573887, 2410178377, 2843173466, 361132917,
        1755816798, 1319204283, 609284796, 1998842567, 1892325921, 223190385, 1483015769,
        2876023365, 3876009312, 3199738344, 491524099, 160383137, 1219178873, 3870310498,
        1114580266, 4279604166, 855339774, 1983818547, 2297848784, 4118592947, 4084409863,
        2225095054, 4215601993, 946447434, 4205503762, 146088676, 778046685, 1876936928,
        3157333726, 2173097090, 3215738813, 4135448234, 1219619643, 1936128689, 2897130162,
        3336043946, 3779039524, 4200886837, 1359380925, 3402593091, 3140713935, 50855190,
        3122065768, 1501584468, 2512255124, 687125154, 2666013386, 837819715, 3057258172,
        3653455791, 2868624990, 322131992, 42534870, 4036564806, 798099710, 3533853670, 190914037,
        3726947981, 2601169403, 602059656, 1365668439, 1918780004, 394790500, 277566007,
        3891847777, 3365421094, 3139612253, 1380519090, 1183088424, 4203794803, 3049949521,
        4214159484, 3446206962, 1875544460, 3207220027, 3288287026, 913535288, 178159620,
        1410694581, 4190575040, 880731713, 1427805121, 404869072, 3413191414, 2865934056,
        2899472677, 4239222733, 688404529, 3923323887, 933651074, 1199453686, 642723732,
        2850614853, 3104368451, 3054041024, 3129913503, 2805843726, 1829781129, 3479062313,
        650272704, 4224852052, 4085038685, 2616580676, 1793860711, 585126334, 2995262791,
        520446536, 3855655015, 1571815563, 2240778227, 2051010344, 1694977983, 788402852,
        1988089041, 2035558649, 1800063056, 1234412692, 2490862867, 417320514, 2415019489,
        3374117797, 136034611, 898704236, 1247106941, 3923519397, 3563607190, 2454738671,
        3522360389, 2672645476, 146828884, 3985140042, 4233949333, 1184742586, 860278824,
        2815489967, 983483427, 3190081845, 3288865305, 3575181235, 1292151129, 4007823805,
        4049420597, 3499391972, 1611182906, 1721268432, 2944249577, 2487212557, 789127738,
        4027610014, 1057334138, 2902720905,
    ];

    let mut rng = MT19937::from_state(N, ZERO_STATE);
    assert_eq!(rng.getrandbits(32), 0xd82c07cd);
    assert_eq!(rng.getrandbits(32), 0x629f6fbe);
    assert_eq!(rng.getrandbits(32), 0xc2094cac);
    assert_eq!(rng.getrandbits(32), 0xe3e70682);
    assert_eq!(rng.getrandbits(32), 0x6baa9455);
    assert_eq!(rng.getrandbits(32), 0x0a5d2f34);
    assert_eq!(rng.getrandbits(32), 0x42485e3a);
    assert_eq!(rng.getrandbits(32), 0xf728b4fa);
    assert_eq!(rng.getrandbits(32), 0x82e2e662);
    assert_eq!(rng.getrandbits(32), 0x7c65c1e5);
}

fn main() {
    let mut breaker = MT19937Breaker::new();
    let vars = (0..N)
        .map(|n| (0..32).map(move |i| format!("mt_{n}_{i}")))
        .flatten()
        .collect::<Vec<_>>();
    println!("{:?}", vars);

    for b in SeqGen(0).take(256 * 79) {
        breaker.feedbit(b);
        let (e, b) = breaker.system.last().unwrap();
        let mut set = HashSet::new();
        match e {
            Expr::Xor(e) => {
                for x in e {
                    match x {
                        Expr::Value(Value::Var(v)) => {
                            set.insert(v.as_str());
                        }
                        _ => panic!("bad eq"),
                    }
                }
                e
            }
            _ => {
                panic!("bad eq")
            }
        };
        // matrix form
        let row = vars
            .iter()
            .map(|x| if set.get(x.as_str()).is_some() { 1 } else { 0 })
            .collect::<Vec<_>>();
        println!("{:?} {}", row, b);
    }
}
