% ==================================================
% Módulo: interfaces.pl
% Propósito: interface de interação com o jogador
% ==================================================

:- module(interfaces, [
    homeScreen/1,
    read_single_key/1,
    process_key/2
]).

:- use_module(library(readutil)).

% Tela inicial do jogo
homeScreen(Screen) :-
    atomic_list_concat([
        '==============================\n',
        '   BEM-VINDO AO TWO CITIES\n',
        '==============================\n',
        'Comandos:\n',
        '  G - Iniciar o jogo\n',
        '  Q - Sair\n',
        '==============================\n',
        'Escolha uma opcao e pressione ENTER:\n'
    ], Screen).

% Lê uma única tecla digitada
read_single_key(Key) :-
    read_line_to_codes(user_input, Codes),
    string_codes(Str, Codes),
    (   Str = "" -> read_single_key(Key)    % se vazio, pede de novo
    ;   string_upper(Str, Upper),
        sub_string(Upper, 0, 1, _, FirstChar), % pega só a primeira letra
        atom_string(Key, FirstChar)
    ).

% Normaliza tecla lida para maiúscula
process_key(Input, UpperKey) :-
    string_upper(Input, UpperStr),
    atom_string(UpperKey, UpperStr).
