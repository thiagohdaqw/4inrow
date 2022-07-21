:- use_module(library(pce)).

drawChar(_, Count) :- Count =< 0, !.
drawChar(Char, Count) :- write(Char), X is Count - 1, drawChar(Char, X).


drawLine(_, X, _, Width, _) :- X == Width, !, nl.
drawLine(Char, X, Y, Width, P) :-
    XXX is X*50, YYY is Y*50,
    send(P,display,new(CI,circle(50)),point(XXX,YYY)),
    plus(X,1,XX),
    drawLine(Char, XX, Y, Width, P).

drawFooter(X,Width) :- X > Width, !, nl.
drawFooter(X,Width) :- write(X), write(' '), plus(X,1,XX), drawFooter(XX, Width).


draw(Width, Height, Y, P) :- Y == Height, !.
draw(Width, Height, Y, P) :- drawLine('_',0,Y,Width,P), plus(Y,1,YY), draw(Width, Height, YY,P).


drawGrid(P) :- draw(10,10,0,P).

clear :- drawChar('\n',80).


player(0,Char) :- Char = 'X'.
player(1,Char)  :- Char = 'O'.


dropColumn(Row,[]) :- Row is 9.
dropColumn(Row,Column) :- last(Column,C), Row is C-1.

drop(Column,Row) :- findall(Y,grid(Column,Y,_), C), dropColumn(Row,C).


main(Turn,P) :- write('Player['),write(Turn),write('] - Informe a coluna: '),
                read(X),
                Column is X-1,
                drop(Column,Row),
                player(Turn, Char),
                assert(grid(Column,Row,Char)),
                clear,
                drawGrid(P),
                T is (Turn+1) mod 2,
                main(T,P).

initGui(P):-
    new(P,picture),
    new(D, window('4 IN A ROW')),
    send(D, size, size(500, 500)),
    send(D, display, P, point(0, 0)),
    send(P,width(500)),
    send(P,height(500)),
    drawGrid(P),
    send(D, open).

:-  dynamic(grid/3),
    initGui(P),
    main(0,P).
