
load :-
	consult(simple),
	consult(tokenizer),
	consult(interpreter),
	simpleShell([]).

simpleShell(Env) :-
	write(' > '),
        getChars(Command),
	tokenize(Command, Tokens),
	evalCommand(Tokens, Env).

evalCommand([exit], _) :-
        write('bye'), nl.
evalCommand([clear], _) :-
       write('cleaning memory...'), nl,
       simpleShell([]).
evalCommand([load | File], Env) :-
       atomListConcat(File, '', FileName),
       tokenizeFile(FileName, Tokens),
       parse(Program, Tokens, []),
       program2env(Program, Env, NewEnv),
       write('program loaded...'), nl,
       simpleShell(NewEnv).
evalCommand(Tokens, Env) :-
       formula(Formula, Tokens, []),
       eval(Formula, Env, Result),
       write(Result), nl,
       simpleShell(Env).

atomListConcat([], Name, Name).
atomListConcat([Part|Rest], SoFar, Name) :-
      atom_concat(SoFar, Part, Partial),
      atomListConcat(Rest, Partial, Name).

getChars(Line) :-
	       get_char(C),
	       continueLine(C,Line).

continueLine('\n', []).
continueLine(C,[C|Line]) :-
	get_char(Char),
	continueLine(Char, Line).

      
