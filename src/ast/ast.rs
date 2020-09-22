use std::fmt::{Debug, Error, Formatter};

// ast

pub enum Expr {
    Number(i32),
    Boolean(bool),
    Identifier(String),
    Function(String, Vec<Box<Expr>>),
    Op(Box<Expr>, Opcode, Box<Expr>),
    ModOp(Opcode, Box<Expr>),
    Error,
}


impl Debug for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Expr::*;
        match *self {
            Number(n) => write!(fmt, "{:?}", n),
            Boolean(b) => write!(fmt, "{:?}", b),
            Identifier(ref id) => write!(fmt, "{}", id),
            Function(ref id, ref args) => write!(fmt, "{}({:?})", id, args),
            Op(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            ModOp(op, ref r) => write!(fmt, "{:?}{:?}", op, r),
            Error => write!(fmt, "error"),
        }
    }
}

#[derive(Copy, Clone)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
    And,
    Or,
    Is,
    LessThen,
    GreaterThen,
    Neg,
}

impl Debug for Opcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Opcode::*;
        match *self {
            Mul => write!(fmt, "*"),
            Div => write!(fmt, "/"),
            Add => write!(fmt, "+"),
            Sub => write!(fmt, "-"),
            And => write!(fmt, "&&"),
            Or => write!(fmt, "||"),
            Is => write!(fmt, "=="),
            LessThen => write!(fmt, "<"),
            GreaterThen => write!(fmt, ">"),
            Neg => write!(fmt, "!"),
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Typedef {
    Unit,
    Bool,
    I32,
}

impl Debug for Typedef {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Typedef::*;
        match *self {
            Unit => write!(fmt, "()"),
            Bool => write!(fmt, "bool"),
            I32 => write!(fmt, "i32"),
        }
    }
}

#[derive(Copy, Clone)]
pub enum ConditionalType {
    If,
    ElseIf,
    Else
}

impl Debug for ConditionalType {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::ConditionalType::*;
        match *self {
            If => write!(fmt, "if"),
            ElseIf => write!(fmt, "else if"),
            Else => write!(fmt, "else"),
        }
    }
}

pub enum Statement {
    Expr(Box<Expr>),
    VarDef(String, Typedef),
    Function(String, Vec<Box<Statement>>, Option<Typedef>, Box<Statement>),
    Block(Vec<Box<Statement>>, Option<Box<Statement>>),
    Definition(bool, Box<Statement>, Box<Expr>),
    Assignment(String, Box<Expr>),
    Conditional(ConditionalType, Option<Box<Expr>>, Box<Statement>, Option<Box<Statement>>),
    WhileLoop(Box<Expr>, Box<Statement>),
    Return(Box<Expr>),
    Program(Vec<Box<Statement>>),
    Error
}

impl Debug for Statement {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Statement::*;
        match *self {
            Expr(ref e) => write!(fmt, "{:?}", e),
            VarDef(ref id, t) => write!(fmt, "{}: {:?}", id, t),
            Function(ref id, ref args, None, ref b) => write!(fmt, "fn {}({:?}) {:?}", id, args, b),
            Function(ref id, ref args, t, ref b) => write!(fmt, "fn {}({:?}) -> {:?} {:?}", id, args, t.unwrap(), b),
            Block(ref stmts, None) => write!(fmt, "{{\n{:?}\n}}", stmts),
            Block(ref stmts, ref ret) => write!(fmt, "{{\n{:?}\n{:?}\n}}", stmts, ret.as_ref().unwrap()),
            Definition(ismut, ref vardef, ref e) => write!(fmt, "let{} {:?} = {:?})", if ismut { " mut" } else { "" }, vardef, e),
            Assignment(ref id, ref e) => write!(fmt, "{} = {:?}", id, e),
            Conditional(t, None, ref bl, None) => write!(fmt, "{:?} {:?}", t, bl),
            Conditional(t, ref e, ref bl, None) => write!(fmt, "{:?} {:?} {:?}", t, e.as_ref().unwrap(), bl),
            Conditional(t, ref e, ref bl, ref n) => write!(fmt, "{:?} {:?} {:?} {:?}", t, e.as_ref().unwrap(), bl, n.as_ref().unwrap()),
            WhileLoop(ref e, ref bl) => write!(fmt, "while ({:?}) {:?}", e, bl),
            Return(ref e) => write!(fmt, "return {:?}", e),
            Program(ref prg) => write!(fmt, "{:?}", prg),
            Error => write!(fmt, "error")
        }
    }
}