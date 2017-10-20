run(StringExpresion, ProgramFile, Result) :-
  tokenizeFile(ProgramFile, TokenList),
  parse(Program, TokenList, []),
  program2env(Program, [], Environment),
  tokenizeString(StringExpresion, Tokens),
  formula(Formula, Tokens, []),
  eval(Formula, Environment, Result).

program2env(nada, Env, Env).
program2env(simple(Def,Resto), List, Env) :-
  program2env(Resto, [Def|List], Env).

find(X, [X|_]) :- !.
find(X, [_|Y]) :- 
  find(X,Y).
find(X, []) :-
  write(X), write(' not defined'), nl, !, fail.

bind(nada, nada, Env, Env) :- !.
bind(identlist(Id,RestId), params(Expr,RestExpr), Env, [def(Id,Binding)|Result]) :-
  eval(Expr, Env, Binding),
  bind(RestId, RestExpr, Env, Result).

evalParams(nada, _, nada) :- !.
evalParams(params(Expr, RestExpr), Env, [R1|R2]) :-
  eval(Expr, Env, R1),
  evalParams(RestExpr, Env, R2).

eval(num(N), _, num(N)) :- !.
eval(val(Ident,nada), _, Ident) :-
  isConstant(Ident), !.
eval(val(Ident,nada), Env, Result) :-
  !,
  find(def(Ident, Expr), Env),
  eval(Expr, Env, Result).
eval(val(Ident,ParamList), Env, val(Ident,Result)) :-
  isConstant(Ident),
  !,
  evalParams(ParamList, Env, Result).
eval(val(Ident,ParamList), Env, Result) :-
  !,
  find(def(Ident, lambda(IdentList, Expresion)), Env),
  bind(IdentList, ParamList, Env, NewEnv),
  eval(Expresion, NewEnv, Result).


eval(sum(Expr1,Expr2), Env, num(Result)) :-
  !,
  eval(Expr1, Env, num(N1)),
  eval(Expr2, Env, num(N2)),
  Result is N1+N2.
eval(sub(Expr1,Expr2), Env, num(Result)) :-
  !,
  eval(Expr1, Env, num(N1)),
  eval(Expr2, Env, num(N2)),
  Result is N1-N2.
eval(mul(Expr1,Expr2), Env, num(Result)) :-
  !,
  eval(Expr1, Env, num(N1)),
  eval(Expr2, Env, num(N2)),
  Result is N1*N2.
eval(div(Expr1,Expr2), Env, num(Result)) :-
  !,
  eval(Expr1, Env, num(N1)),
  eval(Expr2, Env, num(N2)),
  Result is N1 / N2.
eval(cond(Expr1, '<', Expr2), Env, Result) :-
  !,
  eval(Expr1, Env, num(N1)),
  eval(Expr2, Env, num(N2)),
  less(N1,N2,Result).
eval(cond(Expr1, '>', Expr2), Env, Result) :-
  !,
  eval(Expr1, Env, num(N1)),
  eval(Expr2, Env, num(N2)),
  less(N2,N1,Result).
eval(cond(Expr1, '=', Expr2), Env, Result) :-
  !,
  eval(Expr1, Env, N1),
  eval(Expr2, Env, N2),
  equal(N2,N1,Result).

%TODO: Perform logical operations in booleans.
eval(cond(Expr1, 'and', Expr2), Env, Result) :-
  !,
  eval(Expr1, Env, N1),
  eval(Expr2, Env, N2),
  Result is (N1 , N2).

eval(cond(Expr1, 'or', Expr2, Env, Result) :-
  !,
  eval(Expr1, Env, N1),
  eval(Expr2, Env, N2),
  Result is (N1 ; N2).

eval(not(Expr1), Env, Result) :-
  !,
  eval(Expr1, Env, Bool),
  Result \= Bool.

eval(if(Condicion, Then, Else), Env, Result) :-
  !,
  eval(Condicion, Env, Bool),
  branch(Bool, Then, Else, Env, Result).

eval(let(Defs,Exp), Env, Result) :-
  !,
  program2env(Defs,Env,NewEnv),
  eval(Exp,NewEnv,Result).

eval(K,_,K) :-
  isConstant(K), !.
eval(X,_,_) :-
  !,
  write(X), write(' command not defined').

%TODO: Evaluate boolean identifiers to prolog booleans.
eval(X, _, true) :-
  isBoolean(X),
  X = 'true'.

eval(X, _, false) :-
  isBoolean(X),
  X = 'false'.

branch(true, Then, _, Env, Result) :-
  !,
  eval(Then, Env, Result).
branch(false, _, Else, Env, Result) :-
  !,
  eval(Else, Env, Result).

% -- -- %

less(N1, N2, true) :-
  N1 < N2, !.
less(_, _, false).
equal(N,N, true) :- !.
equal(_,_, false).
