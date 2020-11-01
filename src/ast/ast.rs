use std::fmt::{Debug, Error, Formatter};

// ast

#[derive(PartialEq, Clone)]
pub enum Expr {
    Unit,
    Number(i32),
    Boolean(bool),
    Str(String),
    Dereference(Box<Expr>),
    Borrow(bool, Box<Expr>),
    Identifier(String, CodeSpan),
    Function(String, Vec<Box<Expr>>, CodeSpan),
    Op(Box<Expr>, Opcode, Box<Expr>, CodeSpan),
    ModOp(Opcode, Box<Expr>, CodeSpan),
    BlockExpr(Box<Statement>, CodeSpan),
    ConditionalExpr(Box<Statement>, CodeSpan),
    Error,
}

#[derive(PartialEq, Clone, Copy)]
pub struct CodeSpan {
    pub l: usize,
    pub r: usize,
}

impl CodeSpan {
    pub fn new(l: usize, r: usize) -> CodeSpan {
        CodeSpan { l: l, r: r }
    }
}
impl Debug for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Expr::*;
        match *self {
            Unit => write!(fmt, "()"),
            Number(n) => write!(fmt, "{:?}", n),
            Boolean(b) => write!(fmt, "{:?}", b),
            Str(ref s) => write!(fmt, "\"{:?}\"", s),
            Dereference(ref stmt) => write!(fmt, "*{:?}", stmt),
            Borrow(true, ref stmt) => write!(fmt, "&mut {:?}", stmt),       
            Borrow(false, ref stmt) => write!(fmt, "&{:?}", stmt),
            Identifier(ref id, _) => write!(fmt, "{}", id),
            Function(ref id, ref args, _) => write!(fmt, "{}({:?})", id, args),
            Op(ref l, op, ref r, _) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            ModOp(op, ref r, _) => write!(fmt, "{:?}{:?}", op, r),
            BlockExpr(ref stmt, _) => write!(fmt, "{:?}", stmt),
            ConditionalExpr(ref stmt, _) => write!(fmt, "{:?}", stmt),
            Error => write!(fmt, "error"),
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
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

#[derive(Clone, PartialEq)]
pub enum Typedef {
    Implicit,
    Unit,
    Bool,
    I32,
    Str,
    Ref(bool, Box<Typedef>),
}

impl Debug for Typedef {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Typedef::*;
        match *self {
            Unit => write!(fmt, "()"),
            Bool => write!(fmt, "bool"),
            I32 => write!(fmt, "i32"),
            Str => write!(fmt, "String"),
            Implicit => write!(fmt, "implicit"),
            Ref(true, ref t) => write!(fmt, "&mut {:?}", t),
            Ref(false, ref t) => write!(fmt, "&{:?}", t),
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
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

#[derive(Clone, PartialEq)]
pub enum Statement {
    Expr(Box<Expr>, CodeSpan),
    VarDef(String, Typedef),
    Function(String, Vec<Box<Statement>>, Option<Typedef>, Box<Statement>, CodeSpan),
    Block(Vec<Box<Statement>>, Option<Box<Statement>>, CodeSpan),
    Definition(bool, Box<Statement>, Box<Expr>, CodeSpan),
    Assignment(String, Box<Expr>, CodeSpan),
    Conditional(ConditionalType, Option<Box<Expr>>, Box<Statement>, Option<Box<Statement>>, CodeSpan),
    WhileLoop(Box<Expr>, Box<Statement>, CodeSpan),
    Return(Box<Expr>, CodeSpan),
    Program(Vec<Box<Statement>>, CodeSpan),
    Error
}

impl Debug for Statement {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Statement::*;
        match *self {
            Expr(ref e, _) => write!(fmt, "{:?};", e),
            VarDef(ref id, ref t) => write!(fmt, "{}: {:?}", id, t),
            Function(ref id, ref args, None, ref b, _) => write!(fmt, "fn {}({:?}) {:?}", id, args, b),
            Function(ref id, ref args, ref t, ref b, _) => write!(fmt, "fn {}({:?}) -> {:?} {:?}", id, args, t, b),
            Block(ref stmts, None, _) => write!(fmt, "{{\n{:?}\n}}", stmts),
            Block(ref stmts, ref ret, _) => write!(fmt, "{{\n{:?}\n{:?}\n}}", stmts, ret.as_ref().unwrap()),
            Definition(ismut, ref vardef, ref e, _) => write!(fmt, "let{} {:?} = {:?});", if ismut { " mut" } else { "" }, vardef, e),
            Assignment(ref id, ref e, _) => write!(fmt, "{} = {:?};", id, e),
            Conditional(t, None, ref bl, None, _) => write!(fmt, "{:?} {:?}", t, bl),
            Conditional(t, ref e, ref bl, None, _) => write!(fmt, "{:?} {:?} {:?}", t, e.as_ref().unwrap(), bl),
            Conditional(t, ref e, ref bl, ref n, _) => write!(fmt, "{:?} {:?} {:?} {:?}", t, e.as_ref().unwrap(), bl, n.as_ref().unwrap()),
            WhileLoop(ref e, ref bl, _) => write!(fmt, "while ({:?}) {:?}", e, bl),
            Return(ref e, _) => write!(fmt, "return {:?}", e),
            Program(ref prg, _) => write!(fmt, "{:?}", prg),
            Error => write!(fmt, "error")
        }
    }
}