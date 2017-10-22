tokenizeString(String, Tokens) :-
  convertString2Chars(String, Chars),
  tokenize(Chars, [], [], Tokens).

convertString2Chars([],[]) :- !.
convertString2Chars([ASCII|ATail], [Char|CharTail]) :-
  char_code(Char, ASCII),
  convertString2Chars(ATail, CharTail).

readFile(File, Result) :-
  see(File),
  get_char(Char),
  readStream(Char, Result).

readStream(end_of_file, []) :-
  !, seen.
readStream(Char, [Char|Rest]) :-
  get_char(NextChar),
  readStream(NextChar, Rest).

tokenizeFile(File, Result) :-
  readFile(File, List),
  tokenize(List, [], [], Result).

tokenize(Stream, Result) :-
  tokenize(Stream, [], [], Result).

tokenize([], [], L, Result) :- !, reverse(L, Result).
tokenize([], Word, List, Result) :-
  reverse(Word, Tok),
  convert(Tok, Token),
  reverse([Token | List], Result).

tokenize([Char|RemainingChars], Token, List, Result) :-
  updateToken(Char, Token, List, NewToken, NewList),
  eatComment(Char, RemainingChars, NextChars),
  tokenize(NextChars, NewToken, NewList, Result).

eatComment('#', RemainingChars, NextChars) :-
  !, advanceToEndOfLine(RemainingChars, NextChars).
eatComment(_, NextChars, NextChars).

advanceToEndOfLine([],[]) :- !.
advanceToEndOfLine(['\n'|NextLines], NextLines) :- !.
advanceToEndOfLine([_|Rest], NextLines) :- 
  advanceToEndOfLine(Rest, NextLines).

updateToken(Space, Token, List, [], NewList) :-
  member(Space, ['\n', ' ', '\t', '#']), !,
  addToken(Token, List, NewList).
updateToken(SeparatorCHAR, Token, List, [], [Separator|NewList]) :-
  member(SeparatorCHAR, ['(', ')', ',','-','=',':',';','+','-','*','/']), !,
  addToken(Token, List, NewList),
  char_code(SeparatorCHAR, ASCII),
  atom_codes(Separator,[ASCII]).
updateToken(Char, Token, List, [ASCII|Token], List) :- char_code(Char, ASCII).

addToken([], List, List) :- !.
addToken(Word, List, [Token|List]) :-
  reverse(Word, Tok),
  convert(Tok, Token).

convert([Digito|Resto], num(N)) :-
  member(Digito, "0123456789"), !,
  Acum is Digito - 48,
  makeNumber(Resto, Acum, N).
convert(Word, Token) :-
  atom_codes(Token, Word).

makeNumber([], N, N) :- !.
makeNumber([Digito|Resto], Acum, N) :-
  member(Digito, "0123456789"),
  AcumNew is Acum*10 + (Digito-48),
  makeNumber(Resto, AcumNew, N).
makeNumber([Punto|Resto],Acum,N) :-
  member(Punto,"."),
  makeFrac(Resto,Acum,10,N).

makeFrac([],N,_,N).
makeFrac([Digito|Resto], Acum, Pot, N) :-
  member(Digito,"0123456789"),
  AcumNew is Acum + (Digito-48)/Pot,
  makeFrac(Resto,AcumNew, Pot*10, N).

isNumber([],L,L).
isNumber([Digito|Resto],L,L):-
  member(Digito, "0123456789"),
  isNumber(Resto,L,L).


isIdentifier(Word, _, _) :-
  member(Word, ['if', 'then', 'else', 'fun', 'true', 'false', 'and', 'or', 'not']),
  !, fail.

isIdentifier(Atom, L, L) :-
  atom(Atom),
  atom_codes(Atom, List),
  isIdentifier(List).

isIdentifier([A|Resto]) :-
  member(A, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
  isAlphaNum(Resto).

isConstant(Token) :-
  atom_codes(Token, [ASCII|_]),
  member(ASCII, "ABCDEFGHIJKLMNOPQRSTUVWXYZ").

isAlphaNum([]).
isAlphaNum([A|Resto]) :-
  member(A, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"),
  isAlphaNum(Resto).

isBoolean(Token) :-
  member(Token, 'true', 'false'),
  !.