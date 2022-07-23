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

drop(Column,Row) :- findall(Y,grid(Column,Y,_), C),dropColumn(Row,C).


checkHorizontal(Col,_,_,_) :- Col > 9, !, fail.
checkHorizontal(_,_,_,Count) :- Count == 4, !, write('Ganhou Horizontal!'), nl.
checkHorizontal(Col,Row,Turn,Count) :- not(grid(Col,Row,Turn)), !, plus(Col,1,CC), checkHorizontal(CC,Row,Turn,0).
checkHorizontal(Col,Row,Turn,Count) :- plus(Count,1,RS), plus(Col,1,CC), checkHorizontal(CC,Row,Turn,RS).

checkVertical(_,Row,_,_) :- Row < 0, !, fail.
checkVertical(_,_,_,Count) :- Count == 4, !, write('Ganhou Vertical!'), nl.
checkVertical(Col,Row,Turn,Count) :- not(grid(Col,Row,Turn)), !, plus(Row,-1,RR), checkVertical(Col,RR,Turn,0).
checkVertical(Col,Row,Turn,Count) :- plus(Count,1,RS), plus(Row,-1,RR), checkVertical(Col,RR,Turn,RS).

check(Col,Row,X,Y,Turn) :- plus(Col,X,CC),plus(Row,Y,RR),grid(CC,RR,Turn).

checkDiagonalLeft(_,_,_,_,Step) :- Step > 4, !, fail.
checkDiagonalLeft(_,_,_,Count,_) :- Count == 4, !, write('Ganhou Diagonal Esquerda'), nl.
checkDiagonalLeft(Col,Row,Turn,Count,Step) :- not(check(Col,Row,Step,Step,Turn)), !, plus(Step,1,SS), checkDiagonalLeft(Col,Row,Turn,0,SS).
checkDiagonalLeft(Col,Row,Turn,Count,Step) :- plus(Count,1,RS), plus(Step,1,SS), checkDiagonalLeft(Col,Row,Turn,RS,SS).

checkDiagonalRight(_,_,_,_,X,_) :- X > 4, !, fail.
checkDiagonalRight(_,_,_,Count,_,_) :- Count == 4, !, write('Ganhou Diagonal Direita'), nl.
checkDiagonalRight(Col,Row,Turn,_,X,Y) :- not(check(Col,Row,X,Y,Turn)), !, plus(X,1,XX), plus(Y,-1,YY), checkDiagonalRight(Col,Row,Turn,0,XX,YY).
checkDiagonalRight(Col,Row,Turn,Count,X,Y) :- plus(Count,1,RS), plus(X,1,XX), plus(Y,-1,YY), checkDiagonalRight(Col,Row,Turn,RS,XX,YY).

checkWinMove(Col,Row,Turn) :- 
    checkHorizontal(0,Row,Turn,0)
    ; checkVertical(Col,9,Turn,0)
    ; checkDiagonalLeft(Col,Row,Turn,0,-4)
    ; checkDiagonalLeft(Col,Row,Turn,0,-4)
    ; checkDiagonalRight(Col,Row,Turn,0,-4,4).

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
    not(main(0,P)),
    drawChar('\n',5),
    drawChar('=',40).
