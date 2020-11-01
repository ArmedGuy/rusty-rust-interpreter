use crate::ast;
use std::collections::HashMap;

type Error = String;


struct FuncSignature {
    fnbody: Box<ast::Statement>,
    args: Vec<String>,
}

#[derive(Clone)]
struct VarFeatures {
    expr: ast::Expr,
    mutable: bool,
    reference: bool,
    borrowed: bool,
    mutborrowed: bool,
    transferred: bool,
}

pub struct Scope {
    functable: HashMap<i32, HashMap<String, FuncSignature>>,
    vartable: HashMap<i32, HashMap<String, VarFeatures>>,
    fnscopes: Vec<i32>,
    scope: i32,
    source: String,
}

impl Scope {
    pub fn new(source: String) -> Scope {
        let mut s = Scope {functable: HashMap::new(), vartable: HashMap::new(), fnscopes: vec![0], scope: 0, source: source };
        s.functable.insert(0, HashMap::new());
        s.vartable.insert(0, HashMap::new());
        s
    }
    fn push(&mut self, fnscope: bool) {
        self.scope += 1;
        if fnscope {
            self.fnscopes.push(self.scope)
        }
        self.functable.insert(self.scope, HashMap::new());
        self.vartable.insert(self.scope, HashMap::new());
    }
    
    fn pop(&mut self) {
        self.functable.remove(&self.scope);
        self.vartable.remove(&self.scope);
        if self.fnscopes.contains(&self.scope) {
            self.fnscopes.pop();
        }
        self.scope -= 1;
    }

    fn register_function(&mut self, id: &String, statements: ast::Statement, args: Vec<String>) {
        let scope = self.functable.get_mut(&self.scope).unwrap();
        scope.insert(id.to_string(), FuncSignature{ fnbody: Box::new(statements), args: args });
    }

    fn register_variable(&mut self, id: &String, expr: ast::Expr, mutable: bool) {
        let scope = self.vartable.get_mut(&self.scope).unwrap();
        scope.insert(id.to_string(), VarFeatures{
            expr: expr, 
            borrowed: false,
            mutable: mutable,
            reference: false,
            mutborrowed: false,
            transferred: false,
        });
    }

    fn get_var(&mut self, id: &String) -> Result<ast::Expr, Error> {
        let mut current = self.scope;
        let fnscope = *self.fnscopes.last().unwrap();
        while current >= fnscope {
            let scope = self.vartable.get(&current).unwrap();
            if scope.contains_key(id) {
                let var = scope.get(id).unwrap();
                return Ok(var.expr.clone());
            }
            current -= 1;
        }
        Err(format!("variable {} not found in scope", id))
    }

    fn set_var(&mut self, id: &String, expr: ast::Expr) {
        let mut current = self.scope;
        let fnscope = *self.fnscopes.last().unwrap();
        while current >= fnscope {
            let mut scope = self.vartable.get_mut(&current).unwrap();
            if scope.contains_key(id) {
                let mut var = scope.get_mut(id).unwrap();
                var.expr = expr.clone();
            }
            current -= 1;
        }
    }
    fn get_fn(&mut self, id: &String, args: Vec<ast::Expr>) -> Result<ast::Expr, Error> {
        let mut current = self.scope;
        while current >= 0 {
            let scope = self.functable.get(&current).unwrap();
            if scope.contains_key(id) {
                let sign = scope.get(id).unwrap();
                let mut argmap: HashMap<String, ast::Expr> = HashMap::new();
                for (i, aid) in sign.args.iter().enumerate() {
                    argmap.insert(aid.clone(), args.get(i).unwrap().clone());
                }
                return eval_func(self, sign.fnbody.clone(), argmap)
            }
            current -= 1;
        }
        Err(format!("function {}({:?}) not found in scope", id, args))
    }

    fn format_error(&self, err: String, span: ast::CodeSpan) -> Result<ast::Expr, Error> {
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

    fn format_function_if_err(&self, res: Result<ast::Expr, Error>, span: ast::CodeSpan) -> Result<ast::Expr, Error> {
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


pub fn eval_expr(scope: &mut Scope, expr: Box<ast::Expr>) -> Result<ast::Expr, Error> {
    match *expr {
        ast::Expr::Boolean(_) | ast::Expr::Number(_) | ast::Expr::Str(_)=> Ok(*expr),
        ast::Expr::Op(e1, op, e2, span) => {
            let r1 = eval_expr(scope, e1);
            let r2 = eval_expr(scope, e2);
            if r1.is_err() {
                return r1;
            }
            if r2.is_err() {
                return r2;
            }
            let t1 = r1.unwrap();
            let t2 = r2.unwrap();
            match op {
                ast::Opcode::Add => {
                    if let ast::Expr::Number(n1) = t1 {
                        if let ast::Expr::Number(n2) = t2 {
                            return Ok(ast::Expr::Number(n1 + n2))
                        } else {
                            return Err(format!("Unknown error"))
                        }
                    } else {
                        return Err(format!("Unknown error"))
                    }
                },
                ast::Opcode::Sub => {
                    if let ast::Expr::Number(n1) = t1 {
                        if let ast::Expr::Number(n2) = t2 {
                            return Ok(ast::Expr::Number(n1 - n2))
                        } else {
                            return Err(format!("Unknown error"))
                        }
                    } else {
                        return Err(format!("Unknown error"))
                    }
                },
                ast::Opcode::Mul => {
                    if let ast::Expr::Number(n1) = t1 {
                        if let ast::Expr::Number(n2) = t2 {
                            return Ok(ast::Expr::Number(n1 * n2))
                        } else {
                            return Err(format!("Unknown error"))
                        }
                    } else {
                        return Err(format!("Unknown error"))
                    }
                },
                ast::Opcode::Div => {
                    if let ast::Expr::Number(n1) = t1 {
                        if let ast::Expr::Number(n2) = t2 {
                            if n2 == 0 {
                                return Err(format!("Attempted to divide by 0"))
                            }
                            return Ok(ast::Expr::Number(n1 / n2))
                        } else {
                            return Err(format!("Unknown error"))
                        }
                    } else {
                        return Err(format!("Unknown error"))
                    }
                },
                ast::Opcode::GreaterThen => {
                    if let ast::Expr::Number(n1) = t1 {
                        if let ast::Expr::Number(n2) = t2 {
                            return Ok(ast::Expr::Boolean(n1 > n2))
                        } else {
                            return Err(format!("Unknown error"))
                        }
                    } else {
                        return Err(format!("Unknown error"))
                    }
                },
                ast::Opcode::LessThen => {
                    if let ast::Expr::Number(n1) = t1 {
                        if let ast::Expr::Number(n2) = t2 {
                            return Ok(ast::Expr::Boolean(n1 < n2))
                        } else {
                            return Err(format!("Unknown error"))
                        }
                    } else {
                        return Err(format!("Unknown error"))
                    }
                },
                ast::Opcode::Is => {
                    Ok(ast::Expr::Boolean(t1 == t2))
                },
                ast::Opcode::And => {
                    if let ast::Expr::Boolean(b1) = t1 {
                        if let ast::Expr::Boolean(b2) = t2 {
                            return Ok(ast::Expr::Boolean(b1 && b2))
                        } else {
                            return Err(format!("Unknown error"))
                        }
                    } else {
                        return Err(format!("Unknown error"))
                    }
                },
                ast::Opcode::Or => {
                    if let ast::Expr::Boolean(b1) = t1 {
                        if let ast::Expr::Boolean(b2) = t2 {
                            return Ok(ast::Expr::Boolean(b1 || b2))
                        } else {
                            return Err(format!("Unknown error"))
                        }
                    } else {
                        return Err(format!("Unknown error"))
                    }
                },
                _ => Err("swag".to_string())
            }
        },
        ast::Expr::ModOp(ast::Opcode::Neg, e, span) => {
            let r1 = eval_expr(scope, e);
            if r1.is_err() {
                return r1;
            }
            let t1 = r1.unwrap();
            if let ast::Expr::Boolean(b1) = t1 {
                Ok(ast::Expr::Boolean(!b1))
            } else {
                Err(format!("Unknown error"))
            }
        },
        ast::Expr::ModOp(ast::Opcode::Sub, e, span) => {
            let r1 = eval_expr(scope, e);
            if r1.is_err() {
                return r1;
            }
            let t1 = r1.unwrap();
            if let ast::Expr::Number(n1) = t1 {
                Ok(ast::Expr::Number(-n1))
            } else {
                Err(format!("Unknown error"))
            }
        },
        ast::Expr::Function(id, exprs, span) => {
            // TODO typecheck expressions
            let mut args = vec![];
            scope.push(false); // this is cheating, but yolo
            for expr in exprs {
                let r = eval_expr(scope, expr);
                if r.is_err() {
                    scope.pop();
                    return r;
                } else {
                    args.push(r.unwrap());
                }
            }
            
            let r = scope.get_fn(&id, args);
            scope.pop();
            return if r.is_err() {
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
        /*
        ast::Expr::ConditionalExpr(stmt, _) => {
            statement_type_check(scope, vec![stmt])
        },
        ast::Expr::Borrow(mutable, inner) => {
            if let ast::Expr::Identifier(id, span) = *inner {
                let r = scope.get_var(&id);
                if r.is_err() { 
                    scope.format_error(format!("{}", r.unwrap_err()), span)
                } else {
                    let r = scope.borrow_var(&id, mutable);
                    if r.is_err() {
                        scope.format_error(r.unwrap_err(), span)
                    } else {
                        r
                    }
                }
            } else {
                Err(format!("Can only borrow variables"))
            }
        },
        ast::Expr::Dereference(inner) => {
            if let ast::Expr::Identifier(id, span) = *inner {
                let r = scope.get_var(&id);
                if r.is_err() {
                    scope.format_error(format!("{}", r.unwrap_err()), span)
                } else {
                    if let ast::Typedef::Ref(_, inner_type) = r.unwrap() {
                        Ok(*inner_type)
                    } else {
                        scope.format_error(format!("Cannot dereference {}, is not a reference", id), span)
                    }
                }
            } else {
                Err(format!("Can only dereference variables"))
            }
        },*/
        _ => Err(format!("Unrecognized expression {:?}", *expr)),
    }
}
pub fn eval_block(scope: &mut Scope, body: Box<ast::Statement>) -> Result<ast::Expr, Error> {
    scope.push(false);
    let ret = match *body {
        ast::Statement::Block(stmts, None, span) => {
            eval_statements(scope, stmts)
        },
        ast::Statement::Block(stmts, Some(ret2), span) => {
            let chk = eval_statements(scope, stmts);
            if chk.is_err() {
                chk
            } else {
                if let ast::Statement::Return(e, inner_span) = *ret2 {
                    let r = eval_expr(scope, e);
                    println!("func returns {:?}", r);
                    r
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
pub fn eval_func(scope: &mut Scope, body: Box<ast::Statement>, args: HashMap<String, ast::Expr>) -> Result<ast::Expr, Error> {
    // TODO pass function parameters here
    scope.push(true);
    for (k, v) in args.iter() {
        scope.register_variable(&k, v.clone(), false);
    }
    let ret = eval_block(scope, body);
    scope.pop();
    return ret;
}

fn eval_cond(scope: &mut Scope, next: Box<ast::Statement>) -> Result<ast::Expr, Error> {
    match *next {
        ast::Statement::Conditional(ast::ConditionalType::ElseIf, Some(e), body, None, _) => {
            let r = eval_expr(scope, e);
                if r.is_err() {
                    r
                } else {
                    let val = r.unwrap();
                    if let ast::Expr::Boolean(b1) = val {
                        if b1 {
                            eval_block(scope, body)
                        } else {
                            Ok(ast::Expr::Unit)
                        }
                    } else {
                        Err(format!("plz no"))
                    }
                }
        },
        ast::Statement::Conditional(ast::ConditionalType::ElseIf, Some(e), body, Some(next), _) => {
            let r = eval_expr(scope, e);
            if r.is_err() {
                r
            } else {
                let val = r.unwrap();
                if let ast::Expr::Boolean(b1) = val {
                    if b1 {
                        eval_block(scope, body)
                    } else {
                        eval_cond(scope, next)
                    }
                } else {
                    Err(format!("plz no"))
                }
            }
        },
        ast::Statement::Conditional(ast::ConditionalType::Else, None, body, None, _) => {
            eval_block(scope, body)
        },
        _ => Err(format!("Unrecoverable error in cond")),
    }
}

pub fn eval_statements(scope: &mut Scope, stmts: Vec<Box<ast::Statement>>) -> Result<ast::Expr, Error> {
    let stmt_len = stmts.len();
    let mut i = 1;
    let mut unboxed = vec![];
    for stmt in stmts {
        unboxed.push(*stmt);
    }
    // Breadth first, find all functions and register their signatures
    for stmt in &unboxed {
        if let ast::Statement::Function(id, vars, ret, body, _) = stmt {
            let mut args = vec![];
            for var in vars {
                if let ast::Statement::VarDef(id, _) = &**var {
                    args.push(id.clone());
                } else {
                    
                }
            }
            let ret = if ret.is_some() { ret.as_ref().unwrap().clone() } else { ast::Typedef::Unit };
            scope.register_function(&id, *body.clone(), args);
        } else {

        }
    }
    // Depth first, all functions in current scope registered
    for stmt in unboxed {
        let is_last = i == stmt_len;
        let res: Result<ast::Expr, Error> = match stmt {
            ast::Statement::Program(statements, _) => {
                eval_statements(scope, statements)
            },
            ast::Statement::Function(_, vars, Some(ret), body, span) => {
                Ok(ast::Expr::Unit)
            },
            ast::Statement::Function(_, vars, None, body, span) => {
                Ok(ast::Expr::Unit)
            },
            ast::Statement::Block(_, _, span) => eval_block(scope, Box::new(stmt)),
            ast::Statement::Expr(e, _) => {
                eval_expr(scope,e)
            },
            ast::Statement::Definition(mutable, vardef, e, span) => {
                if let ast::Statement::VarDef(id, def) = *vardef {
                    let expr = eval_expr(scope, e);
                    if expr.is_err() {
                        scope.format_error(expr.unwrap_err(), span)
                    } else {
                        scope.register_variable(&id, expr.unwrap(), mutable);
                        Ok(ast::Expr::Unit)
                    }
                } else {
                    Err(format!("Unrecoverable error"))
                }
            },
            ast::Statement::Assignment(id, e, span) => {
                let r1 = scope.get_var(&id);
                if r1.is_err() {
                    r1
                } else {
                    let expr = eval_expr(scope, e);
                    if expr.is_err() {
                        expr
                    } else {
                        scope.set_var(&id, expr.unwrap());
                        Ok(ast::Expr::Unit)
                    }
                }
            },
            ast::Statement::WhileLoop(e, body, span) => {
                loop {
                    let r = eval_expr(scope, e.clone());
                    if r.is_err() {
                        break;
                    } else {
                        let val = r.unwrap();
                        if let ast::Expr::Boolean(b1) = val {
                            if !b1 {
                                break
                            } else {
                                eval_block(scope, body.clone());
                            }
                        } else {
                            break;
                        }
                    }
                }
                Ok(ast::Expr::Unit)
            },
            ast::Statement::Conditional(ast::ConditionalType::If, Some(e), body, None, _) => {
                let r = eval_expr(scope, e);
                if r.is_err() {
                    r
                } else {
                    let val = r.unwrap();
                    if let ast::Expr::Boolean(b1) = val {
                        if b1 {
                            eval_block(scope, body)
                        } else {
                            Ok(ast::Expr::Unit)
                        }
                    } else {
                        Err(format!("plz no"))
                    }
                }
            },
            ast::Statement::Conditional(ast::ConditionalType::If, Some(e), body, Some(next), _) => {
                let r = eval_expr(scope, e);
                if r.is_err() {
                    r
                } else {
                    let val = r.unwrap();
                    if let ast::Expr::Boolean(b1) = val {
                        if b1 {
                            eval_block(scope, body)
                        } else {
                            eval_cond(scope, next)
                        }
                    } else {
                        Err(format!("plz no"))
                    }
                }
            },
            _ => Err(format!("Unrecoverable error in stmt")),
        };
        if res.is_err() {
            return res
        }
        if is_last {
            return res
        } else {
            
        }
        i += 1;
    }
    return Ok(ast::Expr::Unit)
}

pub fn eval_program(scope: &mut Scope, program: Box<ast::Statement>) -> Result<ast::Expr, Error> {
    if let ast::Statement::Program(stmts, _) = *program {
        eval_statements(scope, stmts);
        eval_statements(scope, vec![
            Box::new(
                ast::Statement::Expr(
                Box::new(
                    ast::Expr::Function("main".to_string(), vec![], ast::CodeSpan::new(0, 0))
                ), ast::CodeSpan::new(0, 0))
            )
        ])
    } else {
        Err(format!(""))
    }
}