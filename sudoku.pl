% Sudoku 

solve(Filas) :-
    maplist(dominio19, Filas)
  , diferente(Filas)
  , transpuesta(Filas,Cols), diferente(Cols)
  , bloques(Filas,Bloques) , diferente(Bloques)
  , maplist(fd_labeling, Filas)
  .

dominio19(Filas) :- fd_domain(Filas,1,9).
diferente(Filas)  :- maplist(fd_all_different, Filas).
 
bloques(Filas,Bloques) :-
    maplist(split3,Filas,Xs), transpuesta(Xs,Ys)
  , concatenar(Ys,Zs), concatenarMap(split3,Zs,Bloques)
  . % where
    split3([X,Y,Z|L],[[X,Y,Z]|R]) :- split3(L,R).
    split3([],[]).
 
 
% utils/list
concatenarMap(F,Xs,Ys) :- call(F,Xs,Zs), maplist(concatenar,Zs,Ys).
 
concatenar([],[]).
concatenar([X|Xs],Ys) :- append(X,Zs,Ys), concatenar(Xs,Zs).
 
transpuesta([],[]).
transpuesta([[X]|Col], [[X|Row]]) :- transpuesta(Col,[Row]).
transpuesta([[X|Row]], [[X]|Col]) :- transpuesta([Row],Col).
transpuesta([[X|Row]|Xs], [[X|Col]|Ys]) :-
    maplist(bind_head, Row, Ys, YX)
  , maplist(bind_head, Col, Xs, XY)
  , transpuesta(XY,YX)
  . % where
    bind_head(H,[H|T],T).
    bind_head([],[],[]).
 
sudokuEjemplo(
    [[1,3,3],[2,1,4],[2,5,8],[2,8,3],[2,9,6],[3,3,8],[3,7,1],
     [4,2,4],[4,5,6],[4,8,7],[4,9,3],[5,4,9],[6,6,2],[6,9,5],
     [7,3,4],[7,5,7],[7,8,6],[7,9,8],[8,1,6],[9,1,7],[9,4,6],[9,7,5]]).

 % main([[1,3,3],[2,1,4],[2,5,8],[2,8,3],[2,9,6],[3,3,8],[3,7,1], [4,2,4],[4,5,6],[4,8,7],[4,9,3],[5,4,9],[6,6,2],[6,9,5], [7,3,4],[7,5,7],[7,8,6],[7,9,8],[8,1,6],[9,1,7],[9,4,6],[9,7,5]]).

testEmpty([ [_,_,_,_,_,_,_,_,_]
     , [_,_,_,_,_,_,_,_,_]
     , [_,_,_,_,_,_,_,_,_]
     , [_,_,_,_,_,_,_,_,_]
     , [_,_,_,_,_,_,_,_,_]
     , [_,_,_,_,_,_,_,_,_]
     , [_,_,_,_,_,_,_,_,_]
     , [_,_,_,_,_,_,_,_,_]
     , [_,_,_,_,_,_,_,_,_]
     ]).

cambiarValor([R,C,V], Board) :-
    nth1(R, Board, Row),
    nth1(C, Row, V).

write_list([],Tabla).
write_list([Head|Tail], Tabla):-
    cambiarValor(Head, Tabla),
    write_list(Tail, Tabla).

main([A|B]):- 
    testEmpty(T),
    write_list([A|B],T),
    solve(T),
    maplist(show,T).

show(X) :- write(X), nl.
