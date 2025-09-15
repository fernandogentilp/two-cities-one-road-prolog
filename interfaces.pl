% ==================================================
% Módulo: interfaces.pl
% Propósito: interface do usuário e telas
% ==================================================

:- module(interfaces, [
	homeScreen/1,
	gameScreen/3,
	endScreen/3,
	read_single_key/1,
	process_key/2,
	mapToMatrix/2
        ]).

:- use_module(library(lists)).
:- use_module(mapUtils).
:- use_module(types).
:- use_module(library(readutil)).

% Tela inicial
homeScreen(Screen) :-
    Title = 'Two Cities One Road',
    middleJustifyLine(Title, TopLine),
    
    Lines = [
        'Bem vindo ao jogo!',
        '',
        'Seu objetivo é conectar duas cidades com dinheiro',
        'limitado e diversos obstáculos naturais.',
        '',
        'Durante o jogo, use as teclas de direcao (w, a, s, d)',
        'para construir trilhos nos quadrantes vizinhos ao seu.'
    ],
    topJustifyColumn(Lines, MiddleLines),
    
    Footer = 'G: gerar e jogar novo mapa | Q: sair',
    middleJustifyLine(Footer, BottomLine),
    
    baseScreen(TopLine, MiddleLines, BottomLine, Screen).

% Tela de jogo
gameScreen(Map, Budget, Screen) :-
    string_concat('Dinheiro restante: ', Budget, CashStr),
    string_concat('Posição do jogador: ', '(0,0)', PosStr),

    string_concat(CashStr, ' | ', Temp),
    string_concat(Temp, PosStr, TopText),
    middleJustifyLine(TopText, TopLine),
    
    % Converter mapa para representação visual ampliada
    mapToMatrix(Map, Matrix),
    middleJustifyColumn(Matrix, MiddleLines),
    
    Footer = 'W-A-S-D: construir caminhos | Q: sair',
    middleJustifyLine(Footer, BottomLine),
    
    baseScreen(TopLine, MiddleLines, BottomLine, Screen).

% Tela de fim de jogo
endScreen(Status, Infos, Screen) :-
    middleJustifyLine(Status, TopLine),
    topJustifyColumn(Infos, MiddleLines),
    Footer = 'Q: sair',
    middleJustifyLine(Footer, BottomLine),
    baseScreen(TopLine, MiddleLines, BottomLine, Screen).

% Base para todas as telas
baseScreen(Top, MiddleLines, Bottom, Screen) :-
    simpleLine(SimpleLine),
    
    Line1 = '╭',
    Line2 = '╮',
    Line3 = '├',
    Line4 = '┤',
    Line5 = '╰',
    Line6 = '╯',
    
    atom_concat(Line1, SimpleLine, Temp1),
    atom_concat(Temp1, Line2, FirstLine),
    
    atom_concat('│', Top, Temp2),
    atom_concat(Temp2, '│', SecondLine),
    
    atom_concat(Line3, SimpleLine, Temp3),
    atom_concat(Temp3, Line4, ThirdLine),
    
    middleWithEdges(MiddleLines, MiddleSectionList),
    atomic_list_concat(MiddleSectionList, '\n', MiddleSection),
    
    atom_concat(Line3, SimpleLine, Temp4),
    atom_concat(Temp4, Line4, FifthLine),
    
    atom_concat('│', Bottom, Temp5),
    atom_concat(Temp5, '│', SixthLine),
    
    atom_concat(Line5, SimpleLine, Temp6),
    atom_concat(Temp6, Line6, SeventhLine),
    
    Lines = [
        FirstLine,
        SecondLine,
        ThirdLine,
        MiddleSection,
        FifthLine,
        SixthLine,
        SeventhLine
    ],
    
    atomic_list_concat(Lines, '\n', Screen).

% Linha simples de 55 caracteres
simpleLine(Line) :-
    findall('─', between(1, 55, _), DashList),
    atom_chars(Line, DashList).

% Linha vazia de 55 caracteres
emptyLine(Line) :-
    findall(' ', between(1, 55, _), SpaceList),
    atom_chars(Line, SpaceList).

% Justificar linha no meio
middleJustifyLine(Str, Result) :-
    atom_length(Str, Len),
    Remain is 55 - Len,
    Left is Remain // 2,
    Right is Remain - Left,
    
    findall(' ', between(1, Left, _), LeftSpaces),
    findall(' ', between(1, Right, _), RightSpaces),
    
    atom_chars(Str, StrChars),
    append([LeftSpaces, StrChars, RightSpaces], AllChars),
    atom_chars(Result, AllChars).

% Justificar coluna no meio
middleJustifyColumn(Strs, Result) :-
    length(Strs, LinesNumber),
    LinesNumberBefore is (15 - LinesNumber) // 2,
    LinesNumberAfter is 15 - LinesNumber - LinesNumberBefore,
    
    emptyLines(LinesNumberBefore, Before),
    emptyLines(LinesNumberAfter, After),
    
    maplist(leftJustifyLine, Strs, JustifiedLines),
    
    append([Before, JustifiedLines, After], Result).

% Justificar coluna no topo
topJustifyColumn(Strs, Result) :-
    length(Strs, LinesNumber),
    LinesNumberAfter is 15 - LinesNumber,
    
    emptyLines(LinesNumberAfter, After),
    maplist(leftJustifyLine, Strs, JustifiedLines),
    
    append([JustifiedLines, After], Result).

% Justificar linha à esquerda
leftJustifyLine(Str, Result) :-
    atom_length(Str, Len),
    SpacesNeeded is 55 - Len,
    findall(' ', between(1, SpacesNeeded, _), SpacesList),
    
    atom_chars(Str, StrChars),
    append(StrChars, SpacesList, AllChars),
    atom_chars(Result, AllChars).

% Gerar linhas vazias
emptyLines(0, []) :- !.
emptyLines(N, [Line|Rest]) :-
    N > 0,
    emptyLine(Line),
    N1 is N - 1,
    emptyLines(N1, Rest).

% Adicionar bordas às linhas do meio
middleWithEdges([], []).
middleWithEdges([Line|Lines], [ResultLine|Rest]) :-
    atom_concat('│', Line, Temp),
    atom_concat(Temp, '│', ResultLine),
    middleWithEdges(Lines, Rest).

% ============================================================
% Conversão do mapa em matriz visual AMPLIADA
% ============================================================

mapToMatrix(Map, MatrixRows) :-
    ScaleX = 5,   % Largura de cada tile
    ScaleY = 3,   % Altura de cada tile
    expandMatrix(Map, ScaleX, ScaleY, MatrixRows).

% Expande a matriz original em grid maior
expandMatrix([], _, _, []).
expandMatrix([Row|Rows], ScaleX, ScaleY, Expanded) :-
    expandRow(Row, ScaleX, ExpandedRow),
    duplicateLines(ExpandedRow, ScaleY, ExpandedRowScaled),
    expandMatrix(Rows, ScaleX, ScaleY, ExpandedRows),
    append(ExpandedRowScaled, ExpandedRows, Expanded).

% Expande uma linha (horizontal)
expandRow([], _, []).
expandRow([tile(Terrain,_,_,_,Built)|Rest], ScaleX, [Expanded|ExpandedRest]) :-
    terrainToChar(Terrain, Built, Char),
    expandChar(Char, ScaleX, Expanded),
    expandRow(Rest, ScaleX, ExpandedRest).

% Repete o caractere N vezes (horizontal)
expandChar(Char, ScaleX, Expanded) :-
    atom_chars(Char, [C]),
    findall(C, between(1, ScaleX, _), List),
    atom_chars(Expanded, List).

% Repete uma linha N vezes (vertical)
duplicateLines(_, 0, []) :- !.
duplicateLines(LineList, N, [Line|Rest]) :-
    atomic_list_concat(LineList, '', Line),
    N1 is N - 1,
    duplicateLines(LineList, N1, Rest).

% ============================================================
% Mapeamento de terrenos -> caracteres
% ============================================================

terrainToChar(plains, false, 'p').
terrainToChar(plains, true, 'P').
terrainToChar(mountains, false, 'm').
terrainToChar(mountains, true, 'M').
terrainToChar(lake, false, 'l').
terrainToChar(lake, true, 'L').
terrainToChar(forest, false, 'f').
terrainToChar(forest, true, 'F').
terrainToChar(city, false, 'c').
terrainToChar(city, true, 'C').

% ============================================================
% Entrada de teclas
% ============================================================

read_single_key(Key) :-
    get_single_char(Code),
    char_code(Key, Code).

% Processar tecla em maiúscula
process_key(Key, UpperKey) :-
    char_code(Key, Code),
    (   Code >= 97, Code =< 122  % Letras minúsculas a-z
    ->  UpperCode is Code - 32,   % Converte para maiúscula
        char_code(UpperKey, UpperCode)
    ;   UpperKey = Key
    ).

