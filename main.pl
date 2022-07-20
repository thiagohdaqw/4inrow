drawChar(_, Count) :- Count =< 0, !.
drawChar(Char, Count) :- write(Char), X is Count - 1, drawChar(Char, X).


drawLine(_, X, _, Width) :- X == Width, !.
drawLine(Char, X, Y, Width)  :- grid(X, Y,CElem), write(CElem), plus(X,1,XX), drawLine(Char, XX, Y, Width).
drawLine(Char, X, Y, Width) :- write(Char), plus(X,1,XX), drawLine(Char, XX, Y, Width).


draw(_, Height, Y) :- Y == Height, !.
draw(Width, Height, Y) :- drawLine('_',0,Y,Width), nl, plus(Y,1,YY), draw(Width, Height, YY).


main :- write('Informe o X: '), read(X),
        write('Informe o Y: '), read(Y),
        assert(grid(X,Y,'X')),
        draw(20, 10, 0),
        main.


:-  drawChar('\n',80),
    main,
    halt.