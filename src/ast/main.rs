use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser, "/ast/parser.rs");

use parser::*;

pub mod ast;

fn main() {
    let prg = "fn _if_then_else_and_while() {
        // a function taking two bool arguments returning the bool type
        // with some let statements and function calls
        fn a(x: bool, y: bool) -> bool {
            if x && y {
                let a: bool = true;
                y || a
            } else {
                x && false
            }
        }
    
        // a function taking two bool arguments returning the i32 type
        // with some let statements and function calls
        fn b(x: bool, y: bool) -> i32 {
            let a: bool = a(x, y || false);
            let mut b: i32 = 0;
            if a && y {
                let a: bool = true; // shadowing
                let mut b: bool = false;
                if y || a {
                    b = b + 1;
                };
            } else {
                if !(x && false) {
                    b = b - 1;
                }
            };
            b + 3
        }
    
        // a function taking two bool arguments returning the i32 type
        // while
        fn c(x: bool, y: bool) -> i32 {
            let mut b: i32 = 0;
            let mut c: i32 = 1;
            while (b < 10) {
                c = c * 2;
            }
            c
        }
    }";
    println!("{:?}", ProgramParser::new().parse(prg));

    println!("{:?}", FuncStatementParser::new().parse("fn a(b: i32) { return false }"));
    let test = ProgramParser::new().parse("fn a(b: i32) { return false }").unwrap();
    println!("statement_type_check {:?}", statement_type_check(vec![test]));
}


type Error = String;
type Id = String;

fn expr_type_check(expr: Box<ast::Expr>) -> Result<ast::Typedef, Error> {
    match *expr {
        ast::Expr::Boolean(_) => Ok(ast::Typedef::Bool),
        ast::Expr::Number(_) => Ok(ast::Typedef::I32),
        ast::Expr::Op(e1, op, e2) => {
            let r1 = expr_type_check(e1);
            let r2 = expr_type_check(e2);
            if r1.is_err() {
                return r1;
            }
            if r2.is_err() {
                return r2;
            }
            let t1 = r1.unwrap();
            let t2 = r2.unwrap();
            match op {
                ast::Opcode::Add | ast::Opcode::Sub | ast::Opcode::Div | ast::Opcode::Mul | ast::Opcode::GreaterThen | ast::Opcode::LessThen => {
                    if t1 == ast::Typedef::I32 && t2 == ast::Typedef::I32 {
                        return Ok(ast::Typedef::I32)
                    }
                    return Err(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2))
                },
                ast::Opcode::Is => {
                    if t1 == t2 {
                        return Ok(ast::Typedef::Bool)
                    }
                    return Err(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2))
                },
                ast::Opcode::And | ast::Opcode::Or =>{
                    if t1 == ast::Typedef::Bool && t2 == ast::Typedef::Bool {
                        return Ok(ast::Typedef::Bool)
                    }
                    return Err(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2))
                },
                _ => Err("swag".to_string())
            }
        },
        ast::Expr::ModOp(ast::Opcode::Neg, e) => {
            let r1 = expr_type_check(e);
            if r1.is_err() {
                return r1;
            }
            let t1 = r1.unwrap();
            if t1 == ast::Typedef::Bool {
                return Ok(ast::Typedef::Bool)
            }
            return Err(format!("{:?} does not support negation", t1))
        },
        ast::Expr::Function(id, exprs) => {
            Ok(ast::Typedef::Bool)
        },
        ast::Expr::Identifier(id) => {
            Ok(ast::Typedef::Bool)
        },
        _ => Err("Unrecognized expression".to_string()),
    }
}


fn func_type_check(id: String, ret: ast::Typedef, body: Box<ast::Statement>) -> Result<Id, Error> {
    let ret = match *body {
        ast::Statement::Block(_, None) => {
            if ret == ast::Typedef::Unit {
                Ok("".to_string())
            } else {
                Err(format!("Incompatible return type {:?}, expected {:?}", ast::Typedef::Unit, ret))
            }
        },
        ast::Statement::Block(stmts, Some(ret2)) => {
            let chk = statement_type_check(stmts);
            if chk.is_err() {
                chk
            } else {
                if let ast::Statement::Return(e) = *ret2 {
                    let r1 = expr_type_check(e);
                    if r1.is_err() {
                        Err(format!("Invalid expression, error: {:?}", r1))
                    } else {
                        let t1 = r1.unwrap();
                        if ret == t1 {
                            Ok("".to_string())
                        } else {
                            Err(format!("Incompatible return type {:?}, expected {:?}", t1, ret))
                        }
                    }
                } else {
                    Err(format!("Unrecoverable error"))
                }
            }
        },
        _ => Err(format!("Unrecoverable error")),
    };
    if ret.is_err() {
        return Err(format!("{:?} in function {:?}", ret, id))
    }
    return Ok(id)
}

fn statement_type_check(stmts: Vec<Box<ast::Statement>>) -> Result<Id, Error> {

    for stmt in stmts {
        let res: Result<Id, Error> = match *stmt {
            ast::Statement::Program(statements) => {
                statement_type_check(statements)
            },
            ast::Statement::Function(id, _, Some(ret), body) => {
                func_type_check(id, ret, body)
            },
            ast::Statement::Function(id, _, None, body) => {
                func_type_check(id, ast::Typedef::Unit, body)
            },
            _ => Err(format!("Unrecoverable error")),
        };
        if res.is_err() {
            return res;
        }
    }
    return Ok("".to_string())
}
