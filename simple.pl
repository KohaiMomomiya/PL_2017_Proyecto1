
parse(simple(Definicion, SimpleRest)) --> definicion(Definicion), !, simplerest(SimpleRest).

simplerest(Simple) --> [';'], !, parse(Simple).
simplerest(nada) --> [].

definicion(def(Identificador, Expresion)) --> identificador(Identificador), ['='], !, expresion(Expresion).
definicion(def(Identificador, Expresion)) --> identificador(Identificador), ['='], !, expresionBool(Expresion).

identificador(Identificador) --> [Identificador], isIdentifier(Identificador), !.

expresion(Valor) --> formula(Valor), !.
expresion(Valor) --> lambda(Valor), !.
expresion(Valor) --> let(Valor), !.
expresion(Valor) --> if(Valor), !.

formula(Formula)    --> termino(Termino), !, restoFormula(Termino,Formula).
restoFormula(Acum, Formula) --> ['+'],!,termino(Termino), restoFormula(sum(Acum,Termino),Formula).
restoFormula(Acum, Formula) --> ['-'],!,termino(Termino), restoFormula(sub(Acum,Termino),Formula).
restoFormula(Acum, Acum)    --> [].

termino(Termino) --> factor(Factor), !, restoTermino(Factor,Termino).
restoTermino(Acum, Termino) --> ['*'],!,factor(Factor), restoTermino(mul(Acum,Factor),Termino).
restoTermino(Acum, Termino) --> ['/'],!,factor(Factor), restoTermino(div(Acum,Factor),Termino).
restoTermino(Acum, Acum)    --> [].

factor(num(N))    --> [num(N)], !.
factor(Expresion) --> ['('], !, expresion(Expresion), [')'].
factor(Valor)     --> valor(Valor).

valor(val(Ident, ParamList)) --> identificador(Ident), !, paramlist(ParamList).

paramlist(Params) --> ['('], !, params(Params), [')'].
paramlist(nada)   --> [].

params(params(Expr, RestoParams)) --> expresion(Expr), !, restoParams(RestoParams).

restoParams(Params) --> [','], !, params(Params).
restoParams(nada)   --> [].

lambda(lambda(IdentList, Expresion)) --> ['fun', '('], !, identlist(IdentList), [')', ':'], expresion(Expresion).

identlist(identlist(Ident, IdentRest)) --> identificador(Ident), !, identrest(IdentRest).
identrest(IdentList)  --> [','], !, identlist(IdentList).
identrest(nada)       --> [].

let(let(Defs,Exp)) --> ['let'], !, parse(Defs), ['in'], expresion(Exp).

if(if(Condicion, ThenExpr, ElseExpr)) -->  
  ['if'], !, cond(Condicion), ['then'], !, expresion(ThenExpr), ['else'], !, expresion(ElseExpr).

cond(cond(Expr1, Op, Expr2)) --> expresion(Expr1), opr(Op), expresion(Expr2).

% -- Tipo de dato Boolean (true, false) --

expresionBool(Valor) --> compBool(Valor), !.
expresionBool(Valor) --> constBool(Valor), !.
expresionBool(Valor) --> notBool(Valor), !.
%expresionBool(Valor) --> cond(Valor), !.

compBool(cond(Expr1, Op, Expr2)) --> expresionBool(Expr1), oprBool(Op), expresionBool(Expr2).
constBool(bool(N)) --> [bool(N)], !.
notBool(not(Valor)) --> ['not'], !, expresionBool(Valor).

opr('<') --> ['<'].
opr('=') --> ['='].
opr('>') --> ['>'].

oprBool('and') --> ['and'].
oprBool('or') --> ['or'].

bool('true') --> ['true'].
bool('false') --> ['false'].
