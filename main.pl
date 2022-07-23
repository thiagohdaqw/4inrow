:- use_module(library(pce)).

drawChar(_, Count) :- Count =< 0, !.
drawChar(Char, Count) :- write(Char), X is Count - 1, drawChar(Char, X).


drawLine(_, X, _, Width, _) :- X == Width, !, nl.
drawLine(Char, X, Y, Width, P) :-
    WindowX is X*50, WindowY is Y*50,
    send(P,display,new(CI,circle(50)),point(WindowX,WindowY)),
    plus(X,1,XX),
    drawLine(Char, XX, Y, Width, P).


draw(_, Height, Y, _) :- Y == Height, !.
draw(Width, Height, Y, P) :- drawLine('_',0,Y,Width,P), plus(Y,1,YY), draw(Width, Height, YY,P).

drawGrid(P) :- draw(10,10,0,P).

fillColor(0,CI) :- send(CI,fill_pattern,colour(red)).
fillColor(1,CI) :- send(CI,fill_pattern,colour(blue)).

drawPlayer(Turn, X, Y, P) :- 
    WindowX is X*50, WindowY is Y*50,
    send(P,display,new(CI,circle(50)),point(WindowX,WindowY)),
    fillColor(Turn,CI).

clear :- drawChar('\n',80).


dropColumn(Row,[]) :- Row is 9.
dropColumn(Row,Column) :- last(Column,C), Row is C-1.

drop(Column,Row) :- findall(Y,grid(Column,Y,_), C), dropColumn(Row,C).


checkHorizontal(Col,_,_,_) :- Col > 9, !, fail.
checkHorizontal(_,_,_,Count) :- Count == 4, !, write('Ganhou Horizontal!'), nl.
checkHorizontal(Col,Row,Turn,Count) :- not(grid(Col,Row,Turn)), !, plus(Col,1,CC), checkHorizontal(CC,Row,Turn,0).
checkHorizontal(Col,Row,Turn,Count) :- plus(Count,1,RS), plus(Col,1,CC), checkHorizontal(CC,Row,Turn,RS).

checkWinMove(Col,Row,Turn) :- 
    checkHorizontal(0,Row,Turn,0).

main(Turn,P) :- clear,
                write('Player['),write(Turn),write('] - Informe a coluna: '),
                read(X),
                Column is X-1,
                drop(Column,Row),
                assert(grid(Column,Row,Turn)),
                drawPlayer(Turn,Column,Row,P),
                not(checkWinMove(Column,Row,Turn)), !,
                T is (Turn+1) mod 2,
                main(T,P).

initGui(P):-
    new(P,picture),
    new(D, window('4 IN A ROW')),
    send(D, size, size(500, 500)),
    send(D, display, P, point(0, 0)),
    send(P,width(500)),
    send(P,height(500)),
    send(D, open),
    drawGrid(P).


:-  dynamic(grid/3),
    initGui(P),
    main(0,P).
