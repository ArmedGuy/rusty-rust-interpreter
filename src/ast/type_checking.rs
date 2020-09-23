use crate::ast;
use std::collections::HashMap;

type Error = String;
type Id = String;


pub struct Scope {
    functable: HashMap<i32, HashMap<String, ast::Typedef>>,
    vartable: HashMap<i32, HashMap<String, ast::Typedef>>,
    scope: i32,
}

impl Scope {
    pub fn new() -> Scope {
        let mut s = Scope {functable: HashMap::new(), vartable: HashMap::new(), scope: 0 };
        s.functable.insert(0, HashMap::new());
        s.vartable.insert(0, HashMap::new());
        s
    }
    fn push(&mut self) {
        self.scope += 1;
        self.functable.insert(self.scope, HashMap::new());
        self.vartable.insert(self.scope, HashMap::new());
    }
    
    fn pop(&mut self) {
        self.functable.remove(&self.scope);
        self.vartable.remove(&self.scope);
        self.scope -= 1;
    }

    fn register_function(&mut self, id: &String, ret: ast::Typedef) {
        let scope = self.functable.get_mut(&self.scope).unwrap();
        scope.insert(id.to_string(), ret);
    }

    fn register_variable(&mut self, id: &String, ret: ast::Typedef) {
        let scope = self.vartable.get_mut(&self.scope).unwrap();
        scope.insert(id.to_string(), ret);
    }

    fn get_var(&mut self, id: &String) -> Result<ast::Typedef, Error> {
        let mut current = self.scope;
        while current >= 0 {
            let scope = self.vartable.get(&current).unwrap();
            if scope.contains_key(id) {
                return Ok(*scope.get(id).unwrap());
            }
            current -= 1;
        }
        Err(format!("Variable {} not found in scope.", id))
    }

    fn get_fn(&mut self, id: &String) -> Result<ast::Typedef, Error> {
        let mut current = self.scope;
        while current >= 0 {
            let scope = self.functable.get(&current).unwrap();
            if scope.contains_key(id) {
                return Ok(*scope.get(id).unwrap());
            }
            current -= 1;
        }
        Err(format!("Function {} not found in scope.", id))
    }
}


pub fn expr_type_check(scope: &mut Scope, expr: Box<ast::Expr>) -> Result<ast::Typedef, Error> {
    match *expr {
        ast::Expr::Boolean(_) => Ok(ast::Typedef::Bool),
        ast::Expr::Number(_) => Ok(ast::Typedef::I32),
        ast::Expr::Op(e1, op, e2) => {
            let r1 = expr_type_check(scope, e1);
            let r2 = expr_type_check(scope, e2);
            if r1.is_err() {
                return r1;
            }
            if r2.is_err() {
                return r2;
            }
            let t1 = r1.unwrap();
            let t2 = r2.unwrap();
            match op {
                ast::Opcode::Add | ast::Opcode::Sub | ast::Opcode::Div | ast::Opcode::Mul => {
                    if t1 == ast::Typedef::I32 && t2 == ast::Typedef::I32 {
                        return Ok(ast::Typedef::I32)
                    }
                    return Err(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2))
                },
                ast::Opcode::GreaterThen | ast::Opcode::LessThen => {
                    if t1 == ast::Typedef::I32 && t2 == ast::Typedef::I32 {
                        return Ok(ast::Typedef::Bool)
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
            let r1 = expr_type_check(scope, e);
            if r1.is_err() {
                return r1;
            }
            let t1 = r1.unwrap();
            if t1 == ast::Typedef::Bool {
                return Ok(ast::Typedef::Bool)
            }
            return Err(format!("{:?} does not support negation", t1))
        },
        ast::Expr::ModOp(ast::Opcode::Sub, e) => {
            let r1 = expr_type_check(scope, e);
            if r1.is_err() {
                return r1;
            }
            let t1 = r1.unwrap();
            if t1 == ast::Typedef::I32 {
                return Ok(ast::Typedef::I32)
            }
            return Err(format!("{:?} does not support sign inversion", t1))
        },
        ast::Expr::Function(id, exprs) => {
            // TODO typecheck expressions
            let r = scope.get_fn(&id);
            if r.is_err() {
                Err(format!("{}", r.unwrap_err()))
            } else {
                Ok(r.unwrap())
            }
        },
        ast::Expr::Identifier(id) => {
            let r = scope.get_var(&id);
            if r.is_err() {
                Err(format!("{}", r.unwrap_err()))
            } else {
                Ok(r.unwrap())
            }
        },
        _ => Err(format!("Unrecognized expression {:?}", *expr)),
    }
}

pub fn block_type_check(scope: &mut Scope, body: Box<ast::Statement>) -> Result<ast::Typedef, Error> {
    scope.push();
    let ret = match *body {
        ast::Statement::Block(stmts, None) => {
            statement_type_check(scope, stmts)
        },
        ast::Statement::Block(stmts, Some(ret2)) => {
            let chk = statement_type_check(scope, stmts);
            if chk.is_err() {
                chk
            } else {
                if let ast::Statement::Return(e) = *ret2 {
                    expr_type_check(scope, e)
                } else {
                    Err(format!("Unrecoverable error"))
                }
            }
        },
        _ => Err(format!("Unrecoverable error")),
    };
    scope.pop();
    ret
}

pub fn func_type_check(scope: &mut Scope, id: &String, ret: ast::Typedef, body: Box<ast::Statement>) -> Result<ast::Typedef, Error> {
    let bret = block_type_check(scope, body);
    if bret.is_err() {
        return Err(format!("{} in function {}", bret.unwrap_err(), id))
    } else {
        let r = bret.unwrap();
        if r != ret {
            return Err(format!("Invalid return type {:?}, expected {:?}", r, ret))
        }
    }
    return Ok(ast::Typedef::Unit)
}

fn cond_type_check(scope: &mut Scope, next: Box<ast::Statement>) -> Result<ast::Typedef, Error> {
    match *next {
        ast::Statement::Conditional(ast::ConditionalType::ElseIf, Some(e), body, None) => {
            let r = expr_type_check(scope, e);
                if r.is_err() {
                    Err(format!("Invalid expression in if, error {:?}", r))
                } else {
                    block_type_check(scope, body)
                }
        },
        ast::Statement::Conditional(ast::ConditionalType::ElseIf, Some(e), body, Some(next)) => {
            let r = expr_type_check(scope, e);
            if r.is_err() {
                Err(format!("Invalid expression in if, error {:?}", r))
            } else {
                let r2 = block_type_check(scope, body);
                if r2.is_err() {
                    r2
                } else {
                    let r3 = cond_type_check(scope, next);
                    if r3.is_err() {
                        r3
                    } else {
                        let t2 = r2.unwrap();
                        let t3 = r3.unwrap();
                        if t2 != t3 {
                            Err(format!("Mismatching return type in conditional, expected {:?}, got {:?}", t2, t3))
                        } else {
                            Ok(t3)
                        }
                    }
                }
            }
        },
        ast::Statement::Conditional(ast::ConditionalType::Else, None, body, None) => {
            block_type_check(scope, body)
        },
        _ => Err(format!("Unrecoverable error")),
    }
}

pub fn statement_type_check(scope: &mut Scope, stmts: Vec<Box<ast::Statement>>) -> Result<ast::Typedef, Error> {
    let stmt_len = stmts.len();
    let mut i = 1;
    for stmt in stmts {
        let is_last = i == stmt_len;
        let res: Result<ast::Typedef, Error> = match *stmt {
            ast::Statement::Program(statements) => {
                statement_type_check(scope, statements)
            },
            ast::Statement::Function(id, vars, Some(ret), body) => {
                scope.register_function(&id, ret);
                scope.push();
                for var in vars {
                    if let ast::Statement::VarDef(id, rt) = *var {
                        scope.register_variable(&id, rt);
                    } else {

                    }
                }
                let ret = func_type_check(scope, &id, ret, body);
                scope.pop();
                ret
            },
            ast::Statement::Function(id, vars, None, body) => {
                scope.register_function(&id, ast::Typedef::Unit);
                scope.push();
                for var in vars {
                    if let ast::Statement::VarDef(id, rt) = *var {
                        scope.register_variable(&id, rt);
                    } else {

                    }
                }
                let ret = func_type_check(scope, &id, ast::Typedef::Unit, body);
                scope.pop();
                ret
            },
            ast::Statement::Expr(e) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    Err(format!("Invalid expression, error {:?}", r))
                } else {
                    Ok(ast::Typedef::Unit)
                }
            },
            ast::Statement::Definition(_, vardef, e) => {
                if let ast::Statement::VarDef(id, def) = *vardef {
                    let r = expr_type_check(scope, e);
                    if r.is_err() {
                        Err(format!("Invalid expression, error {:?}", r))
                    } else {
                        let t = r.unwrap();
                        if def == t {
                            scope.register_variable(&id, def);
                            Ok(ast::Typedef::Unit)
                        } else {
                            Err(format!("Expression evaluated to {:?}, expected {:?}", t, def))
                        }
                    }
                } else {
                    Err(format!("Unrecoverable error"))
                }
            },
            ast::Statement::Assignment(id, e) => {
                let r1 = scope.get_var(&id);
                if r1.is_err() {
                    Err(format!("{}", r1.unwrap_err()))
                } else {
                    let r2 = expr_type_check(scope, e);
                    if r2.is_err() {
                        Err(format!("Invalid expression, error {:?}", r2))
                    } else {
                        Ok(ast::Typedef::Unit)
                    }
                }
                //Ok("".to_string()) // todo when lookup table
            },
            ast::Statement::WhileLoop(e, body) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    Err(format!("Invalid expression in while, error {:?}", r))
                } else {
                    let t = r.unwrap();
                    if t != ast::Typedef::Bool {
                        Err(format!("Expression evaluated to {:?}, expected bool", t))
                    } else {
                        let r2 = block_type_check(scope, body);
                        if r2.is_err() {
                            r2
                        } else {
                            let t2 = r2.unwrap();
                            if t2 != ast::Typedef::Unit {
                                Err(format!("Expected {:?} from while, got {:?}", ast::Typedef::Unit, t2))
                            } else {
                                Ok(ast::Typedef::Unit)
                            }
                        }
                    }
                }
            },
            ast::Statement::Conditional(ast::ConditionalType::If, Some(e), body, None) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    Err(format!("Invalid expression in if, error {:?}", r))
                } else {
                    block_type_check(scope, body)
                }
            },
            ast::Statement::Conditional(ast::ConditionalType::If, Some(e), body, Some(next)) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    Err(format!("Invalid expression in if, error {:?}", r))
                } else {
                    let r2 = block_type_check(scope, body);
                    if r2.is_err() {
                        r2
                    } else {
                        let r3 = cond_type_check(scope, next);
                        if r3.is_err() {
                            r3
                        } else {
                            let t2 = r2.unwrap();
                            let t3 = r3.unwrap();
                            if t2 != t3 {
                                Err(format!("Mismatching return type in conditional, expected {:?}, got {:?}", t2, t3))
                            } else {
                                Ok(t3)
                            }
                        }
                    }
                }
            },
            _ => Err(format!("Unrecoverable error")),
        };
        if res.is_err() {
            return res
        }
        if is_last {
            return res
        } else {
            let r = res.unwrap();
            if r != ast::Typedef::Unit {
                return Err(format!("Unexpected return type {:?}, expected Unit", r))
            }
        }
        i += 1;
    }
    return Ok(ast::Typedef::Unit)
}
