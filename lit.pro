% lit.pro 
%   Enable literate Prolog programs, where the file is a markdown text,
%   interspersed with chunks of code.  Small snippets of code can also
%   be embedded in the text inside backquotes.  The program will interpret
%   the code, insert any output and display code as directed.
% 
%   To compile: swipl -o lit -g main_4lit -c lit.pro
%

%%% global declarations %%%
:- ensure_loaded(library(gensym)).
:- dynamic(b_4lit/2).

%%% SECTION TO HANDLE COMMANDLINE ARGUMENTS %%%

main_4lit:-
    catch( (args_4lit(Args), submain_4lit(Args)), Err, (writeln(Err),fail)),
    halt.
main_4lit:- halt(1).

args_4lit(Args):-  
    current_prolog_flag(os_argv,[_,_,_,_|Args]),!.
args_4lit([]):-!.

%! submain(-Args:list) is det.
%   Submain extracts the arguments from the command line and calls the core program
submain_4lit(Args):-
    ( argline_4lit(Infile,Outfile,CodeOnly,Tags,Postproc,Args,_) ->
        lit_4lit(Infile,Outfile,CodeOnly,Tags,Postproc)
    ;   print_arghelp_4lit   % no args at all
    ),!.


%! argline_4lit(?Infile,?Outfile,?CodeOnly,Toks:list,Rest:list) is semidet.
%   The command line argument grammar.
argline_4lit(_,_,_,_,_) --> ['-h'], {print_arghelp_4lit}, !.
argline_4lit(_,_,_,_,_) --> ['--help'], {print_arghelp_4lit}, !.
argline_4lit(Infile,Outfile,true,Tags,Postproc) -->       % extract code only
    ['-x'],
    !, arglinetail_4lit(Infile, Outfile, true, Tags,Postproc).
argline_4lit(Infile,Outfile,CodeOnly,Tags,Postproc) -->
    ['-p',Postproc],
    !, arglinetail_4lit(Infile, Outfile,CodeOnly,Tags,Postproc).
argline_4lit(Infile,Outfile,CodeOnly,Tags,Postproc) -->       % process code with any of these tags only
    ['-t'],[T], 
    {   T\='-x',T\='-o',T\='-h',T\='--help',
        file_name_extension(T,'',T),
        (commaed_4lit(T,Tag) -> Tag=Tag; Tag=T )
    },
    more_tags_4lit(Ts), {append([Tag],Ts,Tags)},
    !, arglinetail_4lit(Infile, Outfile, CodeOnly, Tags,Postproc).
argline_4lit(Infile,Outfile,CodeOnly,Tags,Postproc) -->
    ['-o',Outfile],
    {    not(access_file(Outfile,write)) -> throw('Error: cannot access output file');! },
    !, arglinetail_4lit(Infile, Outfile,CodeOnly,Tags,Postproc).
argline_4lit(Infile,Outfile,CodeOnly,Tags,Postproc) -->
    [Infile],
    {    not(access_file(Infile,read)) -> throw('Error: cannot access input file');! },
    !, arglinetail_4lit(Infile, Outfile,CodeOnly,Tags,Postproc).
arglinetail_4lit(_,_,_,_,_,[],[]):-!.
arglinetail_4lit(In,Out,Code,Tags,Post,S,F):- argline_4lit(In,Out,Code,Tags,Post,S,F).

more_tags_4lit(Tags) --> [','], more_tags_4lit(Tags),!.
more_tags_4lit([Tag|Tags]) --> 
    [T], 
    {   T\='-x',T\='-o',T\='-p',T\='-h',T\='--help',
        file_name_extension(T,'',T),
        (commaed_4lit(T,Tag) -> Tag=Tag; Tag=T )
    },!, 
    more_tags_4lit(Tags).
more_tags_4lit([]) --> {}.

commaed_4lit(Word,Atom):-
    atom_chars(Word,Cs),
    append(As,[','],Cs),
    atom_chars(Atom,As),!.

%! print_arghelp_4lit is det.
%   display the command line help
print_arghelp_4lit:-
    nl,
    writeln('Literate Prolog programs using markdown files.'),
    writeln('usage: lit [-x ] [-p "post processing command"] [-t tag1[, tag2, ..., tagn] ] [-o outputfile] inputfile'),
    writeln('-x  will extract code only into outputfile'),
    writeln('-p "..." is the postprocessing command. default="pandoc --listing -o %w.pdf %w.md", where %w is extensionless filename'),
    writeln('-t tag1[, tag2, ..., tagn]  will only process code blocks with any of these tags'),
    writeln('-o  specify outputfile name. Default is inputfile, but with .md extension, or .pro in case of -x option.'),
    !.

%%% THE CORE PROGRAM STARTS HERE %%%

%! lit_4lit(+Infile:atom, ?Outfile:atom, ?CodeOnly:atom) is det.
%   process the commandline arguments (like opening file handles) and pass to
%   the core program.
lit_4lit(Infile, Outfile, CodeOnly, Partags,Postproc):-
    ( ground(CodeOnly) -> true; CodeOnly = false),
    ( \+ground(Partags) -> Partags=[]; true),
    ( \+ground(Postproc) -> 
        (   member(nonum,Partags) -> 
                Postproc='pandoc -o %w.pdf %w.md'; 
                Postproc='pandoc --listing -o %w.pdf %w.md'
        ); true
    ),
    ( \+ground(Outfile) -> 
        (   file_name_extension(N,E,Infile),
            ( (E=='pro';E=='md') -> 
                throw('Error: input file cannot be .pro or .md to avoid accidental overwriting of source.');
                true
            ),
            (CodeOnly -> Ext='pro'; Ext='md'),
            file_name_extension(N,Ext,Outfile)
        ); true
    ),
    open(Outfile,write, Out,[]),

    (ground(Infile) -> 
        (   open(Infile, read, In,[]),
            current_output(Serr),       % user current output for errors and warnings
            with_output_to(Out,process_lines_4lit(Serr,In,CodeOnly,Partags,text,1)),
            close(Out),
            close(In),
        
            % post processing
            ( \+CodeOnly -> 
                (   file_name_extension(Base,_,Outfile),
                    swritef(Cmd,Postproc,[Base,Base]),
                    shell(Cmd,[])
                ); true
            )
        );   
        print_arghelp_4lit
    ),!.

%! process_lines_4lit(+In:handle,+CodeOnly:atom,+Partags:list,+Lnum:integer) is det.
%   Process: read line and process according to type of line and state of processing
process_lines_4lit(Serr,In,CodeOnly,Partags,State,Lnum):-
    (\+ at_end_of_stream(In)),
    read_line_to_codes(In, Codes),
    aline_4lit(Serr,CodeOnly,Partags,Lnum,Codes,State,Newstate),!,
    Lnext is Lnum + 1,
    process_lines_4lit(Serr,In,CodeOnly,Partags,Newstate,Lnext).
process_lines_4lit(_Serr,_In,_CodeOnly,_Partags,_State,_Lnum).

%! aline_4lit(+CodeOnly:atom,+Lnum:integer,+Chars:list,+State:atom,-Newstate:atom) is semidet.
%   process state transition between different line types:
%   any --(codeblock_start_4lit)--> code     list and evaluate
%   any --(codeblock_start_4lit)--> skip     don't list and don't evaluate
%   any --(codeblock_start_4lit)--> noeval   list but don't evaluate
%   any --(codeblock_start_4lit)--> nolist   evaluated but don't list
%   any --(codeblock_end)--> text
%   S --(other)--> S

aline_4lit(Serr,CodeOnly,Partags,_Lnum,Codes,_,Outstate):-        % codeblock start
    codeblock_start_4lit(CodeOnly,Partags,Outstate,Codes,Rest),
    (   (length(Rest,L), L>0) ->
            (   write(Serr,'Line '), 
                write(Serr,'. Unknown syntax ignored: '),
                string_codes(S,Rest),
                write(Serr,S)
            ); true
    ). 
aline_4lit(_Serr,false,_Partags,_Lnum,Codes,text,text):-         % text line
    \+codeblock_intro_4lit(Codes,_),
    eval_textline_4lit(Cs,Codes,_),
    writef('%s\n',[Cs]).
aline_4lit(_Serr,_CodeOnly,_Partags,_Lnum,Codes,skip,skip):-     % skipped code line
    \+codeblock_intro_4lit(Codes,_).
aline_4lit(_Serr,_CodeOnly,_Partags,Lnum,Codes,nolist,nolist):-  % not listed codeline
    \+codeblock_intro_4lit(Codes,_),
    append_codeline_4lit(Lnum,Codes).
aline_4lit(_Serr,_CodeOnly,_Partags,Lnum,Codes,code,code):-      % listed & evaluated codeline
    \+codeblock_intro_4lit(Codes,_),
    writef('%s\n',[Codes]),
    append_codeline_4lit(Lnum,Codes).
aline_4lit(_Serr,_CodeOnly,_Partags,_Lnum,Codes,noeval,noeval):- % not evaluated codeline
    \+codeblock_intro_4lit(Codes,_),
    writef('%s\n',[Codes]).
aline_4lit(_Serr,CodeOnly,_Partags,_Lnum,Codes,Instate,text):-   % end of codeblock -- evaluate if needed
    codeblock_intro_4lit(Codes,[]),
    ( (\+CodeOnly, Instate\=skip, Instate\=nolist) -> writef('\n~~~\n\n'); true),
    ( (Instate\=skip, Instate\=noeval) -> eval_codeblock_4lit; true).
aline_4lit(_Serr,_CodeOnly,_Partags,_Lnum,_Codes,State,State).


% evaluate an inline clause
% e.g. "The X value is `foo(X), write(X)`" will transform into "The X value is 42"
%      on the assumption that foo(X) evaluates to foo(42).
eval_textline_4lit([C|Cs]) --> [C], {C\=96}, !, eval_textline_4lit(Cs).
eval_textline_4lit(Cs) --> 
    [C], {C=96},                % 96 = '`' (backtick)
    eval_codechunk_4lit(CC),
    {   string_codes(S,CC),
        term_string(T,S),
        with_output_to_codes(call(T),Result)
    },
    eval_textline_4lit(Ctail),!,
    {append(Result,Ctail,Cs)}.
eval_textline_4lit([]) --> {}.

eval_codechunk_4lit([C|Cs]) --> [C], {C\=96}, eval_codechunk_4lit(Cs).
eval_codechunk_4lit([]) -->  [C], {C=96}.


% append line from codeblock to temporary storage
append_codeline_4lit(Lnum,Codes):-
    string_codes(S,Codes),
    assertz(b_4lit(Lnum,S)).

% evaluate current block of code in temp storage
eval_codeblock_4lit:-
    findall(S,b_4lit(_,S),Lines),
    gensym(tmp,ID),
    file_name_extension(ID,pro,Tempfile),
    open(Tempfile,write,F,[]),
    writelnf_4lit(F,Lines),
    close(F),
    consult(Tempfile),
    delete_file(Tempfile),
    retractbeyond_4lit(0).

% write a list of lines to a file with handle F
writelnf_4lit(_,[]):-!.
writelnf_4lit(F,[L|Ls]):-
    writeln(F,L),!,
    writelnf_4lit(F,Ls).

% codeblock line start or end
codeblock_intro_4lit --> [37,37], whitespace_4lit.        % 37='%'

% grammar for line that starts a codeblock
% Grammar: %% mylabel ["mycaption"] [ tag1[, tag2[, tag3...]]]
% Reserved tags: nolist noeval skip nonum
codeblock_start_4lit(CodeOnly,Partags,Outstate) --> 
    codeblock_intro_4lit,
    label_4lit(Label),
    whitespace_4lit,
    caption_4lit(Caption),
    whitespace_4lit, 
    tags_4lit(Tags),
    {   codeblock_state_4lit(Partags,Label,Tags,Outstate), 
        ( (\+CodeOnly, Outstate \= skip, Outstate \= nolist) ->
            (   writef('~~~{ .prolog label=%w caption="%w" numbers=',[Label,Caption]),
                ( (\+member(nonum,Tags),\+member(nonum,Partags)) -> write('left ');write('none ')),
                maplist(write_dottag_4lit,Tags),
                writeln('}')
            );
            true
        )   
    },!.

% Note codeblock_state_4lit are listed in order for efficiency.
codeblock_state_4lit(Partags,Label,Tags,skip):-
    (member(skip,Tags);nopartags_4lit(Partags,[Label|Tags])),!.
codeblock_state_4lit(_,_,Tags,nolist):-
    member(nolist,Tags),!.
codeblock_state_4lit(Partags,_,Tags,noeval):-
    (member(noeval,Tags);member(noeval,Partags)),!.
codeblock_state_4lit(_,_,_,code):-!.

write_dottag_4lit(A):- write('.'), write(A), write(' ').

% whitespace_4lit is spaces or tabs.  End-of-line is handled separately
whitespace_4lit --> [C], {C=9;C=32}, whitespace_4lit.     % 9=tab, 32=space
whitespace_4lit --> {}.

% label_4lit is a sequence of a letter, followed by alphanumerics or underscores
label_4lit(Label) --> [A], {letter_4lit(A)}, label_tail_4lit(As), {atom_chars(Label,[A|As])},!.
label_tail_4lit([A|As]) --> [A], {alpha_num_4lit(A)}, label_tail_4lit(As).
label_tail_4lit([]) --> {}.

lowercase_4lit(C):- C > 96, C < 123.     %  'a' <= C <= 'z'
uppercase_4lit(C):- C > 64, C < 91.      %  'A' <= C <= 'Z'
letter_4lit(C) :- (lowercase_4lit(C); uppercase_4lit(C)),!.
digit_4lit(C):-  C > 47, C < 58.         %  '0' <= C <= '9'
alphanum_4lit(C):- (digit_4lit(C); letter_4lit(C)),!.
alpha_num_4lit(C):- (C=95; alphanum_4lit(C)),!.   % '_'=95

% caption_4lit is any text inside double quotes
caption_4lit(Caption) --> [34], caption_text_4lit(Cap), [34], {atom_chars(Caption,Cap)},!.  % 34='"'
caption_4lit('-') --> {}.
caption_text_4lit([A|As]) --> [A], {A\=34}, caption_text_4lit(As).
caption_text_4lit([]) --> {}.

% tags is a sequence of multiple labels, perhaps separated by commas, semicolons, tabs or spaces
tags_4lit([T|Tags]) --> label_4lit(T), separator_4lit, tags_4lit(Tags).
tags_4lit([]) --> {}.

% separator
separator_4lit --> [C], {C=32;C=9;C=59;C=44}, separator_4lit.  % 32=space, 9=tab, 59=semicolon, 44=comma
separator_4lit --> {},!.

% nopartags succeeds if partags is not empty, but no partag is in Tags
nopartags_4lit([P],Tags):- \+member(P,[nolist,noeval,nonum]),\+member(P,Tags).
nopartags_4lit([P|Ps],Tags):- 
    (member(P,[nolist,noeval,nonum]) -> true; \+member(P,Tags)), 
    nopartags_4lit(Ps,Tags).



%%% UTILITIES %%%

%! retractbeyond_4lit(+H:integer) is semidet
%   remove all b_4lit/2 database entries beyond sequence number H.
retractbeyond_4lit(H):-
    b_4lit(K,_),
    K > H, 
    retract(b_4lit(K,_) ),
    fail.
retractbeyond_4lit(_).
