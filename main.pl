drawChar(_, Count) :- Count =< 0, !.
drawChar(Char, Count) :- write(Char), X is Count - 1, drawChar(Char, X).


drawLine(_, X, _, Width) :- X == Width, !.
drawLine(Char, X, Y, Width) :- grid(X,Y,CElem), write(CElem), plus(X,1,XX), drawLine(Char, XX, Y, Width).
drawLine(Char, X, Y, Width) :- write(Char), plus(X,1,XX), drawLine(Char, XX, Y, Width).


draw(_, Height, Y) :- Y == Height, !.
draw(Width, Height, Y) :- drawLine('_',0,Y,Width), nl, plus(Y,1,YY), draw(Width, Height, YY).


player(0,Char) :- Char = 'X'.
player(1,Char)  :- Char = 'O'.

main(Turn) :-   write('Informe o X: '), read(X),
                write('Informe o Y: '), read(Y),
                player(Turn, Char),
                assert(grid(X,Y,Char)),
                draw(20,10,0),
                T is (Turn+1) mod 2,
                main(T).


:-  drawChar('\n',80),
    main(0),
    halt.