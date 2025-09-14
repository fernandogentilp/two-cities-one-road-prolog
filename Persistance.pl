% filepath: persistence.pl

% --- Valores padrão ---
default_matrix("CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPc").
default_cash("100").
default_coord("00").

file_matrix("data/matrix.txt").
file_cash("data/cash.txt").
file_coord("data/coord.txt").

% --- Verifica e cria arquivos necessários ---
verify_and_create_files :-
    make_directory_path('data'),
    file_matrix(MatrixFile),
    file_cash(CashFile),
    file_coord(CoordFile),
    (   \+ exists_file(MatrixFile) -> default_matrix(M), write_string(MatrixFile, M) ; true ),
    (   \+ exists_file(CashFile)   -> default_cash(C),  write_string(CashFile, C)    ; true ),
    (   \+ exists_file(CoordFile)  -> default_coord(D), write_string(CoordFile, D)   ; true ).

% --- Escreve uma string em um arquivo ---
write_string(File, Content) :-
    open(File, write, Stream),
    write(Stream, Content),
    close(Stream).

% --- Escreve um inteiro em um arquivo ---
write_int(File, Int) :-
    number_string(Int, Str),
    write_string(File, Str).

% --- Lê uma string de um arquivo ---
read_string(File, Content) :-
    exists_file(File),
    open(File, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    string_codes(Content, Codes).

% --- Lê um inteiro de um arquivo ---
read_int(File, Int) :-
    read_string(File, Str),
    number_string(Int, Str).