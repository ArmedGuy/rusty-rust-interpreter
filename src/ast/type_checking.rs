use crate::ast;

type Error = String;
type Id = String;

pub fn expr_type_check(expr: Box<ast::Expr>) -> Result<ast::Typedef, Error> {
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

pub fn block_type_check(ret: ast::Typedef, body: Box<ast::Statement>) -> Result<Id, Error> {
    match *body {
        ast::Statement::Block(stmts, None) => {
            let chk = statement_type_check(stmts);
            if chk.is_err() {
                chk
            } else {
                if ret == ast::Typedef::Unit {
                    Ok("".to_string())
                } else {
                    Err(format!("Incompatible return type {:?}, expected {:?}", ast::Typedef::Unit, ret))
                }
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
    }
}

pub fn func_type_check(id: String, ret: ast::Typedef, body: Box<ast::Statement>) -> Result<Id, Error> {
    let ret = block_type_check(ret, body);
    if ret.is_err() {
        return Err(format!("{} in function {}", ret.unwrap_err(), id))
    }
    return Ok(id)
}

fn cond_type_check(next: Box<ast::Statement>) -> Result<Id, Error> {
    match *next {
        ast::Statement::Conditional(ast::ConditionalType::ElseIf, Some(e), body, None) => {
            let r = expr_type_check(e);
                if r.is_err() {
                    Err(format!("Invalid expression in if, error {:?}", r))
                } else {
                    let r2 = block_type_check(ast::Typedef::Unit, body);
                    if r2.is_err() {
                        r2
                    } else {
                        Ok("".to_string())
                    }
                }
        },
        ast::Statement::Conditional(ast::ConditionalType::ElseIf, Some(e), body, Some(next)) => {
            let r = expr_type_check(e);
            if r.is_err() {
                Err(format!("Invalid expression in if, error {:?}", r))
            } else {
                let r2 = block_type_check(ast::Typedef::Unit, body);
                if r2.is_err() {
                    r2
                } else {
                    cond_type_check(next)
                }
            }
        },
        ast::Statement::Conditional(ast::ConditionalType::Else, None, body, None) => {
            block_type_check(ast::Typedef::Unit, body)
        },
        _ => Err(format!("Unrecoverable error")),
    }
}

pub fn statement_type_check(stmts: Vec<Box<ast::Statement>>) -> Result<Id, Error> {
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
            ast::Statement::Expr(e) => {
                let r = expr_type_check(e);
                if r.is_err() {
                    Err(format!("Invalid expression, error {:?}", r))
                } else {
                    Ok("".to_string())
                }
            },
            ast::Statement::Definition(_, vardef, e) => {
                if let ast::Statement::VarDef(_, def) = *vardef {
                    let r = expr_type_check(e);
                    if r.is_err() {
                        Err(format!("Invalid expression, error {:?}", r))
                    } else {
                        let t = r.unwrap();
                        if def == t {
                            Ok("".to_string())
                        } else {
                            Err(format!("Expression evaluated to {:?}, expected {:?}", t, def))
                        }
                    }
                } else {
                    Err(format!("Unrecoverable error"))
                }
            },
            ast::Statement::Assignment(id, e) => {
                Ok("".to_string()) // todo when lookup table
            },
            ast::Statement::WhileLoop(e, body) => {
                let r = expr_type_check(e);
                if r.is_err() {
                    Err(format!("Invalid expression in while, error {:?}", r))
                } else {
                    let t = r.unwrap();
                    if t != ast::Typedef::Bool {
                        Err(format!("Expression evaluated to {:?}, expected bool", t))
                    } else {
                        block_type_check(ast::Typedef::Unit, body)
                    }
                }
            },
            ast::Statement::Conditional(ast::ConditionalType::If, Some(e), body, None) => {
                let r = expr_type_check(e);
                if r.is_err() {
                    Err(format!("Invalid expression in if, error {:?}", r))
                } else {
                    block_type_check(ast::Typedef::Unit, body)
                }
            },
            ast::Statement::Conditional(ast::ConditionalType::If, Some(e), body, Some(next)) => {
                let r = expr_type_check(e);
                if r.is_err() {
                    Err(format!("Invalid expression in if, error {:?}", r))
                } else {
                    let r2 = block_type_check(ast::Typedef::Unit, body);
                    if r2.is_err() {
                        r2
                    } else {
                        cond_type_check(next)
                    }
                }
            },
            _ => Err(format!("Unrecoverable error")),
        };
        if res.is_err() {
            return res;
        }
    }
    return Ok("".to_string())
}
