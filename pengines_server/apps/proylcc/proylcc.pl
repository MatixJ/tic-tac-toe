:- module(proylcc, 
	[  
		flick/3
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 

flick(Grid, Color, FGrid):-
	Grid = [F|Fs],
	F = [X|Xs],
	Color \= X,
	FGrid = [[Color|Xs]|Fs].

elementoPertenece(X,[X]).
elementoPertenece(X,[X|_]).
elementoPertenece(X,[_|Ys]):-elementoPertenece(X,Ys).

searchCell(position(A,B),Cell,Grid):-
    append(Pre,Pos,Grid),
	length(Pre,B),
	Pos = [X | Xs],
	
	append(PreRow,PosRow, X),
	length(PreRow,A),
	PosRow = [Cell | Cells].

onBounds(position(A,B)):- A>=0,B>=0,A=<6,B=<3.




%sameColorAdjacent(position(A,B),_ListOfAdj,Color,Grid):-
%    searchCell(position(A,B),Cell,Grid),
%    Cell \= Color.

onBounds(L,position(A,B)):- A>=0,B>=0,A=<6,B=<3,elementoPertenece(position(A,B),L).
onBounds(L,position(A,B)):- not(A>=0).
onBounds(L,position(A,B)):- not(B>=0).
onBounds(L,position(A,B)):- not(A=<6).
onBounds(L,position(A,B)):- not(B<3).

validMoves(ValidMovesList,position(A,B)):-
    Up is B-1, Down is B+1, Left is A-1, Right is A+1,
    onBounds(ValidMovesList,position(A,Up)),%Up
    onBounds(ValidMovesList,position(Right,B)),%Right
    onBounds(ValidMovesList,position(A,Down)),%Down
	onBounds(ValidMovesList,position(Left,B)).%Left

%
%sameColorAdjacent(position(A,B),[],Color,Grid,AdjOut).
%sameColorAdjacent(position(A,B),ListOfAdj,Color,Grid,AdjOut).






%
%sameColorAdjacent(position(A,B),_ListOfAdj,_Visited,_Color,_Grid,AdjOut,VisitedOut):- not(onBounds(position(A,B))).

sameColorAdjacent(position(A,B),ListOfAdj,Visited,_Color,_Grid,ListOfAdj,Visited):- 
    onBounds(position(A,B)),
    elementoPertenece(position(A,B), Visited).

sameColorAdjacent(position(A,B),ListOfAdj,Visited,Color,Grid,ListOfAdj,VisitedOut):-
    not(elementoPertenece(position(A,B), Visited)),
    append([position(A,B)],Visited,VisitedOut),
	searchCell(position(A,B),Cell,Grid),
	Cell \= Color.
    

%Caso con 4 adyacentes
sameColorAdjacent(position(A,B),ListOfAdj,Visited,Color,Grid,AdjOut,VisitedOut):-	
    not(elementoPertenece(position(A,B), Visited)),
    
	searchCell(position(A,B),Cell,Grid),
	Cell = Color,
    append([position(A,B)],ListOfAdj,Adjacents1),
    
    validMoves(ValidMovesList,position(A,B)),
	length(ValidMovesList,4),
    
    append([position(A,B)],Visited,Visited1),
    
    nth0(0,ValidMovesList,Pos0),
    nth0(1,ValidMovesList,Pos1),
    nth0(2,ValidMovesList,Pos2),
    nth0(3,ValidMovesList,Pos3),
    
	sameColorAdjacent(Pos0,Adjacents1,Visited1,Color,Grid,Adj1,Visited2),
	sameColorAdjacent(Pos1,Adj1,Visited2,Color,Grid,Adj2,Visited3),
	sameColorAdjacent(Pos2,Adj2,Visited3,Color,Grid,Adj3,Visited4),
	sameColorAdjacent(Pos3,Adj3,Visited4,Color,Grid,AdjOut,VisitedOut).

%Caso con 3 adyacentes
sameColorAdjacent(position(A,B),ListOfAdj,Visited,Color,Grid,AdjOut,VisitedOut):-	
    not(elementoPertenece(position(A,B), Visited)),
    
	searchCell(position(A,B),Cell,Grid),
	Cell = Color,
    
    validMoves(ValidMovesList,position(A,B)),
	length(ValidMovesList,3),
    
    append([position(A,B)],Visited,Visited1),
    
    nth0(0,ValidMovesList,Pos0),
    nth0(1,ValidMovesList,Pos1),
    nth0(2,ValidMovesList,Pos2),
    
	sameColorAdjacent(Pos0,ListOfAdj,Visited1,Color,Grid,Adj1,Visited2),
	sameColorAdjacent(Pos1,Adj1,Visited2,Color,Grid,Adj2,Visited3),
	sameColorAdjacent(Pos2,Adj2,Visited3,Color,Grid,AdjOut,VisitedOut).



%Caso con 2 adyacentes
sameColorAdjacent(position(A,B),ListOfAdj,Visited,Color,Grid,AdjOut,VisitedOut):-	
    not(elementoPertenece(position(A,B), Visited)),
    
	searchCell(position(A,B),Cell,Grid),
	Cell = Color,
    
    validMoves(ValidMovesList,position(A,B)),
	length(ValidMovesList,2),
    
    append([position(A,B)],Visited,Visited1),
    
    nth0(0,ValidMovesList,Pos0),
    nth0(1,ValidMovesList,Pos1),
    
	sameColorAdjacent(Pos0,ListOfAdj,Visited1,Color,Grid,Adj1,Visited2),
	sameColorAdjacent(Pos1,Adj1,Visited2,Color,Grid,AdjOut,VisitedOut).

    
    

sameColorAdjacent(position(A,B),ListOfAdj,_Color,_Grid):-elementoPertenece(position(A,B),ListOfAdj).



searchNReplaceCell(position(A,B),Cell,NewColor,GridIn,GridOut):-
    append(Pre,Pos,GridIn),
	length(Pre,B),
	Pos = [X | Xs],
	
	append(PreRow,PosRow, X),
	length(PreRow,A),
	PosRow = [Cell | Cells],
    
    PosNewRow = [NewColor | Cells],
    append(PreRow,PosNewRow, NewX),
    
    NewPos = [NewX | Xs],
    append(Pre,NewPos,GridOut).

newGridGenerator(GridOut,[],_NewColor,GridOut).
newGridGenerator(GridIn,[X],NewColor,GridOut):-
    searchNReplaceCell(X,_Cell,NewColor,GridIn,GridOut).

newGridGenerator(GridIn,PositionList,NewColor,GridOut):-
    PositionList = [X | Xs],
    searchNReplaceCell(X,_Cell,NewColor,GridIn,AuxGrid),
    newGridGenerator(AuxGrid,Xs,NewColor,GridOut).



