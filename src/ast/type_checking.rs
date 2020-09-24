use crate::ast;
use std::collections::HashMap;

type Error = String;


struct FuncSignature {
    args: Vec<ast::Typedef>,
    ret: ast::Typedef,
}

pub struct Scope {
    functable: HashMap<i32, HashMap<String, FuncSignature>>,
    vartable: HashMap<i32, HashMap<String, ast::Typedef>>,
    scope: i32,
    source: String,
}

impl Scope {
    pub fn new(source: String) -> Scope {
        let mut s = Scope {functable: HashMap::new(), vartable: HashMap::new(), scope: 0, source: source };
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

    fn register_function(&mut self, id: &String, args: Vec<ast::Typedef>, ret: ast::Typedef) {
        let scope = self.functable.get_mut(&self.scope).unwrap();
        scope.insert(id.to_string(), FuncSignature{args: args, ret: ret });
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
        Err(format!("variable {} not found in scope", id))
    }

    fn get_fn(&mut self, id: &String, args: Vec<ast::Typedef>) -> Result<ast::Typedef, Error> {
        let mut current = self.scope;
        while current >= 0 {
            let scope = self.functable.get(&current).unwrap();
            if scope.contains_key(id) {
                let sign = scope.get(id).unwrap();
                let matching = args.iter().zip(sign.args.iter()).filter(|&(a, b)| a == b).count();
                if matching == args.len() && matching == sign.args.len() {
                    return Ok(sign.ret);
                }
            }
            current -= 1;
        }
        Err(format!("function {}({:?}) not found in scope", id, args))
    }

    fn format_error(&self, err: String, span: ast::CodeSpan) -> Result<ast::Typedef, Error> {
        let mut len = 0;
        let starts = self.source.to_string().into_bytes().into_iter()
            .inspect(|_| len+=1)
            .enumerate()
            .filter(|&(_, b)| b == b'\n')
            .map(|(i, _)| i+1);
        
        let mut last = 0;
        for (line, start) in starts.enumerate() {
            // line starts at 0, so using "next" line is perfectly fine
            if span.l < start {
                let column = span.l - last;
                let src = &self.source[last..start-1];
                let bad_part_pointer = format!("{}{}", " ".repeat(column), "^".repeat(span.r - last - column));
                return Err(format!(" - On line {}:{}\n{}\n{} {}\n", line, column + 1, src, bad_part_pointer, err))
            }
            last = start
        }
        Err(format!("Could not locate source position for error"))
    }

    fn format_function_if_err(&self, res: Result<ast::Typedef, Error>, span: ast::CodeSpan) -> Result<ast::Typedef, Error> {
        if res.is_ok() {
            return res
        }
        let mut len = 0;
        let starts = self.source.to_string().into_bytes().into_iter()
            .inspect(|_| len+=1)
            .enumerate()
            .filter(|&(_, b)| b == b'\n')
            .map(|(i, _)| i+1);
        
        let mut last = 0;
        for (line, start) in starts.enumerate() {
            // line starts at 0, so using "next" line is perfectly fine
            if span.l < start {
                let src = &self.source[last..start-1];
                return Err(format!(" | In function on line {}\n | {}\n | \n{}", line, src, res.unwrap_err()))
            }
            last = start
        }
        Err(format!("Could not locate source position for error"))
    }
}


pub fn expr_type_check(scope: &mut Scope, expr: Box<ast::Expr>) -> Result<ast::Typedef, Error> {
    match *expr {
        ast::Expr::Boolean(_) => Ok(ast::Typedef::Bool),
        ast::Expr::Number(_) => Ok(ast::Typedef::I32),
        ast::Expr::Op(e1, op, e2, span) => {
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
                    return scope.format_error(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2), span)
                },
                ast::Opcode::GreaterThen | ast::Opcode::LessThen => {
                    if t1 == ast::Typedef::I32 && t2 == ast::Typedef::I32 {
                        return Ok(ast::Typedef::Bool)
                    }
                    return scope.format_error(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2), span)
                },
                ast::Opcode::Is => {
                    if t1 == t2 {
                        return Ok(ast::Typedef::Bool)
                    }
                    return scope.format_error(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2), span)
                },
                ast::Opcode::And | ast::Opcode::Or =>{
                    if t1 == ast::Typedef::Bool && t2 == ast::Typedef::Bool {
                        return Ok(ast::Typedef::Bool)
                    }
                    return scope.format_error(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2), span)
                },
                _ => Err("swag".to_string())
            }
        },
        ast::Expr::ModOp(ast::Opcode::Neg, e, span) => {
            let r1 = expr_type_check(scope, e);
            if r1.is_err() {
                return r1;
            }
            let t1 = r1.unwrap();
            if t1 == ast::Typedef::Bool {
                return Ok(ast::Typedef::Bool)
            }
            return scope.format_error(format!("{:?} does not support negation", t1), span)
        },
        ast::Expr::ModOp(ast::Opcode::Sub, e, span) => {
            let r1 = expr_type_check(scope, e);
            if r1.is_err() {
                return r1;
            }
            let t1 = r1.unwrap();
            if t1 == ast::Typedef::I32 {
                return Ok(ast::Typedef::I32)
            }
            return scope.format_error(format!("{:?} does not support sign inversion", t1), span)
        },
        ast::Expr::Function(id, exprs, span) => {
            // TODO typecheck expressions
            let mut args = vec![];
            for expr in exprs {
                let r = expr_type_check(scope, expr);
                if r.is_err() {
                    return r;
                } else {
                    args.push(r.unwrap());
                }
            }
            let r = scope.get_fn(&id, args);
            if r.is_err() {
                scope.format_error(format!("{}", r.unwrap_err()), span)
            } else {
                Ok(r.unwrap())
            }
        },
        ast::Expr::Identifier(id, span) => {
            let r = scope.get_var(&id);
            if r.is_err() {
                scope.format_error(format!("{}", r.unwrap_err()), span)
            } else {
                Ok(r.unwrap())
            }
        },
        ast::Expr::ConditionalExpr(stmt, _) => {
            statement_type_check(scope, vec![stmt])
        }
        _ => Err(format!("Unrecognized expression {:?}", *expr)),
    }
}

pub fn block_type_check(scope: &mut Scope, body: Box<ast::Statement>) -> (Result<ast::Typedef, Error>, ast::CodeSpan) {
    scope.push();
    let ret = match *body {
        ast::Statement::Block(stmts, None, span) => {
            (statement_type_check(scope, stmts), span)
        },
        ast::Statement::Block(stmts, Some(ret2), span) => {
            let chk = statement_type_check(scope, stmts);
            if chk.is_err() {
                (chk, span)
            } else {
                if let ast::Statement::Return(e, inner_span) = *ret2 {
                    (expr_type_check(scope, e), inner_span)
                } else {
                    (Err(format!("Unrecoverable error")), span)
                }
            }
        },
        _ => (Err(format!("Unrecoverable error")), ast::CodeSpan::new(0, 0)),
    };
    scope.pop();
    ret
}

pub fn func_type_check(scope: &mut Scope, ret: ast::Typedef, body: Box<ast::Statement>) -> Result<ast::Typedef, Error> {
    let (bret, span) = block_type_check(scope, body);
    if bret.is_err() {
        return bret
    } else {
        let r = bret.unwrap();
        if r != ret {
            return scope.format_error(format!("Invalid return type {:?}, expected {:?}", r, ret), span)
        }
    }
    return Ok(ast::Typedef::Unit)
}

fn cond_type_check(scope: &mut Scope, next: Box<ast::Statement>) -> Result<ast::Typedef, Error> {
    match *next {
        ast::Statement::Conditional(ast::ConditionalType::ElseIf, Some(e), body, None, _) => {
            let r = expr_type_check(scope, e);
                if r.is_err() {
                    r
                } else {
                    block_type_check(scope, body).0
                }
        },
        ast::Statement::Conditional(ast::ConditionalType::ElseIf, Some(e), body, Some(next), _) => {
            let r = expr_type_check(scope, e);
            if r.is_err() {
                r
            } else {
                let (r2, inner_span) = block_type_check(scope, body);
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
                            scope.format_error(format!("Mismatching return type in conditional, expected {:?}, got {:?}", t3, t2), inner_span)
                        } else {
                            Ok(t3)
                        }
                    }
                }
            }
        },
        ast::Statement::Conditional(ast::ConditionalType::Else, None, body, None, _) => {
            block_type_check(scope, body).0
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
            ast::Statement::Program(statements, _) => {
                statement_type_check(scope, statements)
            },
            ast::Statement::Function(id, vars, Some(ret), body, span) => {
                let mut args = vec![];
                for var in &vars {
                    if let ast::Statement::VarDef(_, t) = **var {
                        args.push(t);
                    } else {
                        
                    }
                }
                scope.register_function(&id, args, ret);
                scope.push();
                for var in vars {
                    if let ast::Statement::VarDef(id, rt) = *var {
                        scope.register_variable(&id, rt);
                    } else {

                    }
                }
                let ret = func_type_check(scope, ret, body);
                scope.pop();
                scope.format_function_if_err(ret, span)
            },
            ast::Statement::Function(id, vars, None, body, span) => {
                let mut args = vec![];
                for var in &vars {
                    if let ast::Statement::VarDef(_, t) = **var {
                        args.push(t);
                    } else {
                        
                    }
                }
                scope.register_function(&id, args, ast::Typedef::Unit);
                scope.push();
                for var in vars {
                    if let ast::Statement::VarDef(id, rt) = *var {
                        scope.register_variable(&id, rt);
                    } else {

                    }
                }
                let ret = func_type_check(scope, ast::Typedef::Unit, body);
                scope.pop();
                scope.format_function_if_err(ret, span)
            },
            ast::Statement::Expr(e, _) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    r
                } else {
                    Ok(ast::Typedef::Unit)
                }
            },
            ast::Statement::Definition(_, vardef, e, span) => {
                if let ast::Statement::VarDef(id, def) = *vardef {
                    let r = expr_type_check(scope, e);
                    if r.is_err() {
                        r
                    } else {
                        let t = r.unwrap();
                        if def == t || def == ast::Typedef::Implicit {
                            scope.register_variable(&id, t);
                            Ok(ast::Typedef::Unit)
                        } else {
                            scope.format_error(format!("Expression evaluated to {:?}, expected {:?}", t, def), span)
                        }
                    }
                } else {
                    Err(format!("Unrecoverable error"))
                }
            },
            ast::Statement::Assignment(id, e, span) => {
                let r1 = scope.get_var(&id);
                if r1.is_err() {
                    scope.format_error(format!("{}", r1.unwrap_err()), span)
                } else {
                    let r2 = expr_type_check(scope, e);
                    if r2.is_err() {
                       r2
                    } else {
                        Ok(ast::Typedef::Unit)
                    }
                }
                //Ok("".to_string()) // todo when lookup table
            },
            ast::Statement::WhileLoop(e, body, span) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    r
                } else {
                    let t = r.unwrap();
                    if t != ast::Typedef::Bool {
                        scope.format_error(format!("Expression evaluated to {:?}, expected bool", t), span)
                    } else {
                        let r2 = block_type_check(scope, body).0;
                        if r2.is_err() {
                            r2
                        } else {
                            let t2 = r2.unwrap();
                            if t2 != ast::Typedef::Unit {
                                scope.format_error(format!("Expected {:?} from while, got {:?}", ast::Typedef::Unit, t2), span)
                            } else {
                                Ok(ast::Typedef::Unit)
                            }
                        }
                    }
                }
            },
            ast::Statement::Conditional(ast::ConditionalType::If, Some(e), body, None, _) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    r
                } else {
                    block_type_check(scope, body).0
                }
            },
            ast::Statement::Conditional(ast::ConditionalType::If, Some(e), body, Some(next), _) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    r
                } else {
                    let (r2, inner_span) = block_type_check(scope, body);
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
                                scope.format_error(format!("Mismatching return type in conditional, expected {:?}, got {:?}", t3, t2), inner_span)
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
