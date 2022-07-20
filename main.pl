drawChar(_, Count) :- Count =< 0, !.
drawChar(Char, Count) :- write(Char), X is Count - 1, drawChar(Char, X).


drawLine(_, X, _, Width) :- X == Width, !.
drawLine(Char, X, Y, Width) :- grid(X,Y,CElem), write(CElem), plus(X,1,XX), drawLine(Char, XX, Y, Width), !.
drawLine(Char, X, Y, Width) :- write(Char), plus(X,1,XX), drawLine(Char, XX, Y, Width).


draw(_, Height, Y) :- Y == Height, !.
draw(Width, Height, Y) :- drawLine('_',0,Y,Width), nl, plus(Y,1,YY), draw(Width, Height, YY).


drawGrid :- draw(20,10,0).

clear :- drawChar('\n',80).


player(0,Char) :- Char = 'X'.
player(1,Char)  :- Char = 'O'.


dropColumn(Row,[]) :- Row is 9.
dropColumn(Row,Column) :- last(Column,C), Row is C-1.

drop(Column,Row) :- findall(Y,grid(Column,Y,_), C), dropColumn(Row,C).


main(Turn) :-   write('Player['),write(Turn),write('] - Informe a coluna: '),
                read(Column),
                drop(Column,Row),
                player(Turn, Char),
                assert(grid(Column,Row,Char)),
                clear,
                drawGrid,
                T is (Turn+1) mod 2,
                main(T).


:-  dynamic(grid/3),
    clear,
    drawGrid,
    main(0),
    halt.
