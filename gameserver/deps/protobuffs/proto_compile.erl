-module(proto_compile).

-ifdef(TEST).
-compile(export_all).
-else.
-export([scan_file/1, scan_file/2, scan_string/2, scan_string/3,
	 generate_source/1, generate_source/2]).
-endif.

-record(collected,{enum=[], msg=[], extensions=[], package}).

%%--------------------------------------------------------------------
%% @doc Generats a built .beam file and header file .hrl
%%--------------------------------------------------------------------
-spec scan_file(ProtoFile :: string()) ->
		       ok | {error, _}.
scan_file(ProtoFile) ->
    scan_file(ProtoFile,[]).

-spec scan_string(String :: string(), BaseName :: string()) ->
			 ok | {error, _}.
scan_string(String,BaseName) ->
    scan_string(String,BaseName,[]).

%%--------------------------------------------------------------------
%% @doc Generats a source .erl file and header file .hrl
%%      Considerd option properties: output_include_dir,
%%                                   output_ebin_dir,
%%                                   imports_dir
%%--------------------------------------------------------------------
-spec scan_file(ProtoFile :: string() | atom(), Options :: list()) ->
		       ok | {error, _}.
scan_file(ProtoFile,Options) when is_list(ProtoFile) ->
    {ok,String} = parse_file(ProtoFile),
    parse_and_gen_file(String);
scan_file(ProtoFile,Options) when is_atom(ProtoFile) ->
    {ok,String} = parse_file(atom_to_list(ProtoFile) ++ ".proto"),
    parse_and_gen_file(String).

scan_dir(SrcDir, OutDir) ->
	MsgList = filelib:fold_files(SrcDir, ".+\.proto", false, fun(FileName, Acc) ->
																	 case catch parse_file_and_string(FileName) of
																		 {ok, TList} ->
																			 TList ++ Acc;
																		 Reason ->
																			 exit({Reason, FileName})
																	 end
					   end, 														   
														   []),
	parse_and_gen_file(MsgList, OutDir).

parse_file_and_string(FileName) ->
	{ok, TList} = parse_file(FileName),
	parse_string(TList).

parse_and_gen_file(TokenList) ->
	parse_and_gen_file(TokenList, ".").
parse_and_gen_file(TokenList, OutDir) ->
    %{ok, TokenList} = parse_string(String),
	check_duplicate(TokenList),
	Tokens = sort(TokenList),
    Hrl = gen_hrl(Tokens),
	CppH = gen_cpp_h(Tokens),
	%% 用没排序过的msg列表来生成
	RouteFile = gen_route_file(TokenList),
    EncodeDef = [gen_encode_def(Tokens),encode_def_tail_clause()],
    DecodeDef = [gen_decode_def(Tokens),decode_def_tail_clause()],
	GetID = gen_get_id(Tokens),
    file:write_file(filename:join([OutDir, "all_proto.hrl"]),Hrl, [{encoding,utf8}]),
    file:write_file(filename:join([OutDir, "TK_DefineProto.h"]),CppH, [{encoding,utf8}]),
	file:write_file(filename:join([OutDir, "proto_route.erl"]),RouteFile, [{encoding,utf8}]),
    file:write_file(filename:join([OutDir, "proto_struct.erl"]),[all_proto_erl_header(), EncodeDef, DecodeDef, GetID], [{encoding,utf8}]).

%% 判断列表中是否有重复项
check_duplicate2(List) ->
	Set = sets:from_list(List),
	case length(List) =:= sets:size(Set) of
		true ->
			ok;
		false ->
			Duplicate = List -- sets:to_list(Set),
			exit({duplicate, Duplicate})
	end.

check_duplicate(TokenList) ->
	MsgIDList = [MsgID||{message, _MsgStr, MsgID, _Route,_FieldList}<-TokenList],
	check_duplicate2(MsgIDList),
	MsgNameList = [MsgStr||{message, MsgStr, _MsgID, _Route,_FieldList}<-TokenList],
	check_duplicate2(MsgNameList).
	
gen_route_file(TokenList) ->
	Header = "-module(proto_route).\n"
			"-compile(export_all).\n\n",
	Body = 
	lists:reverse(lists:foldl(fun({message, MsgStr, _, Route,_FieldList}, Acc) ->
						case MsgStr of
							"cs_" ++ MsgStr2 ->
								{ModStr, _}= lists:splitwith(fun(E) -> E =/= $_ end, MsgStr2),
								Server = Route,
								HandleModule = "role_"++ModStr,
								Case = ["route(",MsgStr,") ->\n\t{",Server, ",", HandleModule,"};\n"],
								[Case | Acc];								
							_ ->
								Acc
						end
				end, [], TokenList)),
	Tail = "route(_) ->undefined.\n",
	[Header, Body, Tail].

	
sort(TokenList) ->
	sort(TokenList, [],sets:new()).

-define(TypeList, ["int8","int16","int32","int64","bool","string","any"]).


sort([Msg|List]=List0, Result, AlreadySorted) when element(1,Msg) =:= message->
	RelyOn = rely_on(Msg),
	TheType = element(2,Msg),
	case check_rely(RelyOn, AlreadySorted) of
		[] ->
			sort(List, [Msg|Result], sets:add_element(TheType, AlreadySorted));
		RelyList ->
			List2 = del_rely(RelyList, List0),
			sort(List2, Result, AlreadySorted)
	end;
sort([_|List], Result, AlreadySorted) ->
	sort(List, Result, AlreadySorted);
sort([], Result, _AlreadySorted) ->
	lists:reverse(Result).


del_rely(RelyList, List) ->
	{List2, DelList} = 
	lists:foldl(fun(E, {ListAcc, DelAcc}) ->
						case lists:keytake(E, 2, ListAcc) of
							false ->
								exit({"cannot find type define",E}),
								{ListAcc, DelAcc};
							{value, Del, ListAcc2} ->
								{ListAcc2, [Del|DelAcc]}
						end
				end, {List, []}, RelyList),
	DelList ++ List2.

rely_on({message,_,_,_,Fields}) ->
	sets:to_list(sets:from_list([Type || {_,_,Type,_,_} <- Fields, not lists:member(Type, ?TypeList)])).
	
check_rely(RelyOn, AlreadySorted) ->
	lists:filter(fun(E) -> not sets:is_element(E, AlreadySorted) end, RelyOn). 

gen_hrl(String) ->
    [gen_hrl2(Name, Fields)||{message, Name, _MsgID, _Route, Fields} <- String].
gen_hrl2(Name, Fields) ->
    Head = "-record(" ++ Name ++",{\n\t",
    Body = 
    string:join(
        [
        [
        FieldName
        ,"="
        ,io_lib:format("~1000p",[DefaultValue])
		," :: "
		,erlang_type(RepeatedType, Type)]
        ||{_Number, RepeatedType, Type, FieldName, DefaultValue}<-Fields]
        , "\n\t,"
        ),
    Tail = "}).\n",
    [Head,Body,Tail].

gen_cpp_h(String) ->
	Head = 
"//
//  TK_DefineProto.h
//  ProjectTK
//
//  Created by Zhou Xingyou on 13-2-27.
//
//

#ifndef ProjectTK_TK_DefineProto_h
#define ProjectTK_TK_DefineProto_h

",
	Tail = "
#endif
",

	Body = [["#define ", string:to_upper(Name), " ", integer_to_list(MsgID),"\n"]
			||{message, Name, MsgID, _Route, _Fields} <- String],
[Head, Body, Tail].

erlang_type(repeated, Type) ->
	"["++erlang_type2(Type)++"]";
erlang_type(_, Type) ->
	erlang_type2(Type).

erlang_type2("int8") ->
	"?int8";
erlang_type2("int16") ->
	"?int16";
erlang_type2("int32") ->
	"?int32";
erlang_type2("int64") ->
	"?int64";
erlang_type2("string") ->
	"?string";
erlang_type2("bool") ->
	"boolean()";
erlang_type2("any") ->
	"any()";
erlang_type2(Other) ->
	"#" ++ Other ++ "{}".
	
all_proto_erl_header() ->
"-module(proto_struct).
-compile(export_all).

string(Str) ->
	Str2 = iolist_to_binary(Str),
	[<<(byte_size(Str2)):16>>, Str2].

-define(int8(V), <<V:8>>).
-define(int16(V), <<V:16>>).
-define(int32(V), <<V:32>>).
-define(int64(V), <<V:64>>).
-define(bool(V), (case V of true -> <<1:8>>; false -> <<0:8>> end)).
-define(string(V), (string(V))).
-define(tuple(V), (encode_def(element(1,V),V))).
-define(any(V), (if is_integer(V) ->
						if abs(V) < 128 ->
							   [<<241:8>>,?int8(V)];
						   abs(V) < 32768 ->
							   [<<242:8>>,?int16(V)];
						   abs(V) < 2147483648 ->
							   [<<243:8>>,?int32(V)];
						   true ->
							   [<<244:8>>,?int64(V)]
						end;
					is_boolean(V) ->
						[<<245:8>>,?bool(V)];
					is_list(V) orelse is_binary(V) ->
						[<<246:8>>,?string(V)];
					true ->
						[<<(get_id(element(1,V))):16>>,?tuple(V)]
				 end)).

-define(list_int8(List),	[<<(length(List)):16>>, [ ?int8(E) || E<-List ] ]).
-define(list_int16(List),	[<<(length(List)):16>>, [ ?int16(E) || E<-List ]]).
-define(list_int32(List), 	[<<(length(List)):16>>, [ ?int32(E) || E<-List ]]).
-define(list_int64(List), 	[<<(length(List)):16>>, [ ?int64(E) || E<-List ]]).
-define(list_bool(List), 	[<<(length(List)):16>>, [ ?bool(E) || E <- List ]]).
-define(list_string(List),	[<<(length(List)):16>>, [ ?string(E) || E<-List ]]).
-define(list_tuple(List),	[<<(length(List)):16>>, [ ?tuple(E) || E<-List ]]).
-define(list_any(List),		[<<(length(List)):16>>, [ ?any(E) || E<-List ]]).

".

gen_encode_def(String) ->
    [gen_encode_def2(Name, MsgID, Fields) || {message, Name, MsgID, _Route, Fields} <- String].
gen_encode_def2(Name, MsgID, Fields) ->
    Head = "encode_def("++Name++", R)->\n\t",
	Length = length(Fields),
	Vars = ["{_", [[",V",integer_to_list(N)]||N<-lists:seq(2,Length+1)], "}=R,\n"],
    Body = ["[",
%% 			integer_to_list(MsgID),
%% 			":16",
string:join(
			[
			 ["?",gen_encode_def3(RepeatedType,Type),"(V",integer_to_list(N+1),")"]
			||
			 N<-lists:seq(1,Length), 
			 {_Number, RepeatedType, Type, _FieldName, _DefaultValue}<-[lists:nth(N,Fields)]
			], ","),
			"];\n"
		   ],		
    [Head,
	 Vars, 
	 "\t",
	 Body].

gen_get_id(String) ->
    [ [ ["get_id(",Name,")->",integer_to_list(MsgID),";\n"] || {message, Name, MsgID, _Route, Fields} <- String], "get_id(_)->0."].

encode_def_tail_clause() ->
    "encode_def(_, _) -> <<>>.\n\n".

type(Type)->
	case lists:member(Type, ?TypeList) of
		true ->
			Type;
		false ->
			"tuple"
	end.

gen_encode_def3(repeated, Type) ->
    "list_"++type(Type);
gen_encode_def3(_RepeatedType, Type) ->
    type(Type).

gen_decode_def(String) ->
	TypeIDDict = dict:from_list([{Name,MsgID}||{message, Name, MsgID, _Route, _Fields} <- String]),
    [gen_decode_def2(Name, MsgID, Fields, TypeIDDict) || {message, Name, MsgID, _Route, Fields} <- String].
gen_decode_def2(Name, MsgID, Fields, TypeIDDict) ->
    Head = "decode_def("++integer_to_list(MsgID)++")->\n\t[",
    Body = string:join([Name| 
        [gen_decode_def3(RepeatedType, Type, TypeIDDict)||{_Number, RepeatedType, Type, _FieldName, _DefaultValue}<-Fields]
        ],
        ","
        ),
    Tail = "];\n",
    [Head,Body,Tail].

gen_decode_def3(repeated, Type, TypeIDDict) ->
    "{list, "++ encode_type(Type, TypeIDDict) ++"}";
gen_decode_def3(_RepeatedType, Type, TypeIDDict) ->
    encode_type(Type, TypeIDDict).


encode_type(Type, TypeDict)->
	case lists:member(Type, ?TypeList) of
		true ->
			Type;
		false ->
			integer_to_list(dict:fetch(Type, TypeDict))
	end.
decode_def_tail_clause() ->
    "decode_def(_) -> [].\n\n".

-spec scan_string(String :: string(), Basename :: string(), Options :: list()) ->
			 ok | {error, _}.
scan_string(String,Basename,Options) ->
    {ok,FirstParsed} = parse_string(String),
    ImportPaths = ["./", "src/" | proplists:get_value(imports_dir, Options, [])],
    Parsed = parse_imports(FirstParsed, ImportPaths),
    Collected = collect_full_messages(Parsed),
    Messages = resolve_types(Collected#collected.msg,Collected#collected.enum),
    output(Basename, Messages, Collected#collected.enum, Options).

%%--------------------------------------------------------------------
%% @doc Generats a source .erl file and header file .hrl
%%--------------------------------------------------------------------
-spec generate_source(ProtoFile :: string() | atom() ) ->
			     ok | {error, _}.
generate_source(ProtoFile) ->
  generate_source(ProtoFile,[]).

%%--------------------------------------------------------------------
%% @doc Generats a source .erl file and header file .hrl
%%      Consider option properties: output_include_dir,
%%                                  output_src_dir,
%%                                  imports_dir
%%--------------------------------------------------------------------
-spec generate_source(ProtoFile :: string() | atom(), Options :: list()) ->
			     ok | {error, _}.
generate_source(ProtoFile,Options) when is_atom(ProtoFile) ->
    generate_source(atom_to_list(ProtoFile) ++ ".proto", Options);
generate_source(ProtoFile,Options) when is_list(ProtoFile) ->
    Basename = filename:basename(ProtoFile, ".proto") ++ "_pb",
    {ok,String} = parse_file(ProtoFile),
    {ok,FirstParsed} = parse_string(String),
    ImportPaths = ["./", "src/" | proplists:get_value(imports_dir, Options, [])],
    Parsed = parse_imports(FirstParsed, ImportPaths),
    Collected = collect_full_messages(Parsed),
    Messages = resolve_types(Collected#collected.msg,Collected#collected.enum),
    output_source(Basename, Messages, Collected#collected.enum, Options).

%% @hidden
parse_imports(Parsed, Path) ->
    parse_imports(Parsed, Path, []).

%% @hidden
parse_imports([], _Path, Acc) ->
    lists:reverse(Acc);
parse_imports([{import, File} = Head | Tail], Path, Acc) ->
    case protobuffs_file:path_open(Path, File, [read]) of
	{ok, F, Fullname} ->
	    file:close(F),
	    {ok,String} = parse_file(Fullname),
	    {ok,FirstParsed} = parse_string(String),
	    Parsed = lists:append(FirstParsed, [file_boundary | Tail]),
	    parse_imports(Parsed, Path, [Head | Acc]);
	{error, Error} ->
	    error_logger:error_report([
				       "Could not do import",
				       {import, File},
				       {error, Error},
				       {path, Path}
				      ]),
	    parse_imports(Tail, Path, [Head | Acc])
    end;
parse_imports([Head | Tail], Path, Acc) ->
    parse_imports(Tail, Path, [Head | Acc]).

%% @hidden
output(Basename, MessagesRaw, RawEnums, Options) ->
    Messages = canonize_names(MessagesRaw),
    Enums = canonize_names(RawEnums),
    case proplists:get_value(output_include_dir,Options) of
	undefined ->
	    HeaderFile = Basename ++ ".hrl";
	HeaderPath ->
	    HeaderFile = filename:join(HeaderPath,Basename) ++ ".hrl"
    end,

    error_logger:info_msg("Writing header file to ~p~n",[HeaderFile]),
    ok = write_header_include_file(HeaderFile, Messages),
    PokemonBeamFile = code:where_is_file("pokemon_pb.beam"),
    {ok,{_,[{abstract_code,{_,Forms}}]}} = beam_lib:chunks(PokemonBeamFile, [abstract_code]),
    Forms1 = filter_forms(Messages, Enums, Forms, Basename, []),
    {ok, _, Bytes, _Warnings} = protobuffs_file:compile_forms(Forms1, proplists:get_value(compile_flags,Options,[])),
    case proplists:get_value(output_ebin_dir,Options) of
	undefined ->
	    BeamFile = Basename ++ ".beam";
	BeamPath ->
	    BeamFile = filename:join(BeamPath,Basename) ++ ".beam"
    end,
    error_logger:info_msg("Writing beam file to ~p~n",[BeamFile]),
    protobuffs_file:write_file(BeamFile, Bytes).

%% @hidden
output_source(Basename, MessagesRaw, Enums, Options) ->
    Messages = canonize_names(MessagesRaw),
    case proplists:get_value(output_include_dir,Options) of
	undefined ->
	    HeaderFile = Basename ++ ".hrl";
	HeaderPath ->
	    HeaderFile = filename:join(HeaderPath,Basename) ++ ".hrl"
    end,
    error_logger:info_msg("Writing header file to ~p~n",[HeaderFile]),
    ok = write_header_include_file(HeaderFile, Messages),
    PokemonBeamFile = filename:dirname(code:which(?MODULE)) ++ "/pokemon_pb.beam",
    {ok,{_,[{abstract_code,{_,Forms}}]}} = beam_lib:chunks(PokemonBeamFile, [abstract_code]),
    Forms1 = filter_forms(Messages, Enums, Forms, Basename, []),
    case proplists:get_value(output_src_dir,Options) of
	undefined ->
	    SrcFile = Basename ++ ".erl";
	SrcPath ->
	    SrcFile = filename:join(SrcPath,Basename) ++ ".erl"
    end,
    error_logger:info_msg("Writing src file to ~p~n",[SrcFile]),
    protobuffs_file:write_file(SrcFile, erl_prettypr:format(erl_syntax:form_list(Forms1))).

%% @hidden
parse_file(FileName) ->
    {ok, InFile} = protobuffs_file:open(FileName, [read]),
    String = parse_file(InFile,[]),
    file:close(InFile),
    {ok,String}.

%% @hidden
parse_file(InFile,Acc) ->
    case protobuffs_file:request(InFile) of
        {ok,Token,_EndLine} ->
            parse_file(InFile,Acc ++ [Token]);
        {error,token} ->
            exit(scanning_error);
        {eof,_} ->
            Acc
    end.

parse_string(String) ->
    case lists:all(fun erlang:is_integer/1, String) of
        true ->
            {ok, Tokens, _Line} = protobuffs_scanner:string(String),
            protobuffs_parser:parse(Tokens);
        false ->
            protobuffs_parser:parse(String)
    end.

%% @hidden
filter_forms(Msgs, Enums, [{attribute,L,file,{_,_}}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,file,{"src/" ++ Basename ++ ".erl",L}}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,module,pokemon_pb}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,module,list_to_atom(Basename)}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,export,[{encode_pikachu,1},{decode_pikachu,1}]}|Tail], Basename, Acc) ->
    Exports = lists:foldl(
		fun({Name,_,_}, Acc1) ->
			[{list_to_atom("encode_" ++ string:to_lower(Name)),1},
			 {list_to_atom("decode_" ++ string:to_lower(Name)),1} | Acc1]
		end, [], Msgs),
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,export,Exports}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,record,{pikachu,_}}|Tail], Basename, Acc) ->
    Records = [begin
		   OutFields = [string:to_lower(A) || {_, _, _, A, _} <- lists:keysort(1, Fields)],
       ExtendField = case Extends of
           disallowed -> [];
           _ -> [{record_field,L,{atom,L,'$extensions'}}]
       end,
		   Frm_Fields = [{record_field,L,{atom,L,list_to_atom(OutField)}}|| OutField <- OutFields] ++ ExtendField,
		   {attribute, L, record, {atomize(Name), Frm_Fields}}
	       end || {Name, Fields,Extends} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Records ++ Acc);

filter_forms(Msgs, Enums, [{function,L,encode_pikachu,1,[Clause]}|Tail], Basename, Acc) ->
    Functions = [begin
		     {function,L,list_to_atom("encode_" ++ string:to_lower(Name)),1,[replace_atom(Clause, pikachu, atomize(Name))]}
		 end || {Name, _, _} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Functions ++ Acc);

filter_forms(Msgs, Enums, [{function,L,encode,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_encode_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,encode_extensions,1,[EncodeClause,Catchall]}|Tail], Basename, Acc) ->
    NewEncodeClauses = [replace_atom(EncodeClause, pikachu, atomize(Name)) ||
        {Name, _Fields, Extens} <- Msgs, Extens =/= disallowed],
    NewClauses = NewEncodeClauses ++ [Catchall],
    NewFunction = {function,L,encode_extensions,1,NewClauses},
    filter_forms(Msgs, Enums, Tail, Basename, [NewFunction | Acc]);

filter_forms(Msgs, Enums, [{function,L,iolist,2,[Clause]}|Tail], Basename, Acc) ->
     filter_forms(Msgs, Enums, Tail, Basename, [expand_iolist_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,decode_pikachu,1,[Clause]}|Tail], Basename, Acc) ->
    Functions = [begin
		     {function,
		      L,
		      list_to_atom("decode_" ++ string:to_lower(Name)),
		      1,
		      [replace_atom(Clause, pikachu, atomize(Name))]}
		 end || {Name, _, _} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Functions ++ Acc);

filter_forms(Msgs, Enums, [{function,L,decode,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_decode_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,to_record,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_to_record_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,enum_to_int,2,[Clause]}|Tail], Basename, Acc) ->
    Acc2 = case any_message_has_fields(Msgs) orelse any_message_has_extentions(Msgs) of
        true -> [expand_enum_to_int_function(Enums,L,Clause)|Acc];
        false -> Acc
    end,
    filter_forms(Msgs, Enums, Tail, Basename, Acc2);

filter_forms(Msgs, Enums, [{function,L,int_to_enum,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_int_to_enum_function(Enums, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,decode_extensions,1,[Clause,Catchall]}|Tail],Basename, Acc) ->
    NewClauses = filter_decode_extensions_clause(Msgs, Msgs, Clause, []),
    NewHead = {function,L,decode_extensions,1,NewClauses ++ [Catchall]},
    filter_forms(Msgs, Enums, Tail,Basename,[NewHead|Acc]);

filter_forms(Msgs, Enums, [{function,L,extension_size,1,[RecClause,CatchAll]}|Tail],Basename, Acc) ->
    NewRecClauses = filter_extension_size(Msgs, RecClause, []),
    NewClauses = lists:reverse([CatchAll | NewRecClauses]),
    NewHead = {function,L,extension_size,1,NewClauses},
    filter_forms(Msgs, Enums, Tail, Basename, [NewHead|Acc]);

filter_forms(Msgs, Enums, [{function,L,has_extension,2,[FilterClause,CatchallClause]}|Tail],Basename,Acc) ->
    NewRecClauses = filter_has_extension(Msgs, FilterClause, []),
    NewClauses = lists:reverse([CatchallClause | NewRecClauses]),
    NewHead = {function,L,has_extension,2,NewClauses},
    filter_forms(Msgs, Enums, Tail, Basename, [NewHead | Acc]);

filter_forms(Msgs, Enums, [{function,L,get_extension,2,[AtomClause,IntClause,Catchall]}|Tail],Basename,Acc) ->
    NewAtomClauses = filter_get_extension_atom(Msgs,AtomClause,[]),
    NewRecClauses = filter_get_extension_integer(Msgs, IntClause, NewAtomClauses),
    NewClauses = lists:reverse([Catchall | NewRecClauses]),
    NewHead = {function,L,get_extension,2,NewClauses},
    filter_forms(Msgs,Enums, Tail, Basename, [NewHead | Acc]);

filter_forms(Msgs, Enums, [{function,L,set_extension,3,[RecClause,Catchall]}|Tail],Basename, Acc) ->
    NewRecClauses = filter_set_extension(Msgs, RecClause, []),
    NewClauses = lists:reverse([Catchall | NewRecClauses]),
    NewHead = {function,L,set_extension,3,NewClauses},
    filter_forms(Msgs,Enums,Tail,Basename,[NewHead|Acc]);

filter_forms(Msgs, Enums, [{function,_L,with_default,2,_Args}=Func|Tail],Basename,Acc) ->
    Acc2 = case any_message_has_fields(Msgs) of
        true -> [Func|Acc];
        false -> Acc
    end,
    filter_forms(Msgs,Enums,Tail,Basename,Acc2);

filter_forms(Msgs, Enums, [{function,_L,pack,5,_Clauses}=Func|Tail],Basename,Acc) ->
    Acc2 = case any_message_has_fields(Msgs) orelse any_message_has_extentions(Msgs) of
        true -> [Func|Acc];
        false -> Acc
    end,
    filter_forms(Msgs,Enums,Tail,Basename,Acc2);

filter_forms(Msgs, Enums, [Form|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [Form|Acc]);

filter_forms(_, _, [], _, Acc) -> lists:reverse(Acc).

any_message_has_extentions(Msgs) ->
    Predicate = fun({_,_,disallowed}) -> false; (_) -> true end,
    lists:any(Predicate, Msgs).

any_message_has_fields(Msgs) ->
    Predicate = fun({_,[],_}) -> false; (_) -> true end,
    lists:any(Predicate, Msgs).

%% @hidden
filter_set_extension([],_,Acc) ->
    Acc;
filter_set_extension([{_,_,disallowed}|Tail],Clause,Acc) ->
    filter_set_extension(Tail,Clause,Acc);
filter_set_extension([{MsgName,_,Extends}|Tail],Clause,Acc) ->
    {clause,L,[OldRecArg,_OldAtomArg,ValueArg],Gs,[OldSet,OldReturn]} = Clause,
    {match,L,{record,L,_,RecArgFields},RecVar} = OldRecArg,
    {match,L2,NewReturn,OldDictStore} = OldSet,
    {call,L2,DictStore,[_StoreKey,_StoreVal,StoreVar]} = OldDictStore,
    {tuple,L3,[Ok, OldReturnRec]} = OldReturn,
    {record,L3,ReturnRecVar,OldName,Fields} = OldReturnRec,
    Folder = fun({Id, Rule, StrType, Name, Opts}, Facc) ->
        Type = atomize(StrType),
        FClause = {clause,L,[{match,L,{record,L,atomize(MsgName),RecArgFields},RecVar},{atom,L,atomize(Name)},ValueArg],Gs,[
            {match,L2,NewReturn,{call,L2,DictStore,[{integer,L2,Id},{tuple,L2,[erl_parse:abstract(Rule),ValueArg,erl_parse:abstract(Type),erl_parse:abstract(Opts)]},StoreVar]}},
            {tuple,L3,[Ok,{record,L3,ReturnRecVar,atomize(MsgName),Fields}]}
        ]},
        [FClause | Facc]
    end,
    NewAcc = lists:foldl(Folder, Acc, Extends),
    filter_set_extension(Tail,Clause,NewAcc).

%% @hidden
filter_get_extension_atom([],_AtomClause,Acc) ->
    Acc;
filter_get_extension_atom([{_,_,disallowed}|Tail],Clause,Acc) ->
    filter_get_extension_atom(Tail,Clause,Acc);
filter_get_extension_atom([{Msg,_,Extends}|Tail],Clause,Acc) ->
    {clause,L,[RecArg,_OldAtom],[RecG],[OldSubcall]} = Clause,
    [{call,L,Guard,[Garg1,_RecName]}] = RecG,
    {call,L1,Subname,[RecVar,_OldInt]} = OldSubcall,
    NewG = [{call,L,Guard,[Garg1,{atom,L,atomize(Msg)}]}],
    NewClauses = [
        {clause,L,[RecArg,{atom,L,atomize(FName)}],[NewG],[
          {call,L1,Subname,[RecVar,{integer,L1,FId}]}
        ]} ||
      {FId, _, _, FName, _} <- Extends],
    NewAcc = lists:append(NewClauses,Acc),
    filter_get_extension_atom(Tail,Clause,NewAcc).

%% @hidden
filter_get_extension_integer([],_,Acc) ->
    Acc;
filter_get_extension_integer([{_,_,disallowed}|Tail],IntClause,Acc) ->
    filter_get_extension_integer(Tail,IntClause,Acc);
filter_get_extension_integer([{Msg,_,Extends}|Tail],IntClause,Acc) ->
    {clause,L,[{record,L,Pikachu,Fields},IntArg],Gs,Body} = IntClause,
    NewRecName = replace_atom(Pikachu, pikachu, atomize(Msg)),
    NewRecArg = {record,L,NewRecName,Fields},
    NewClause = {clause,L,[NewRecArg,IntArg],Gs,Body},
    NewAcc = [NewClause|Acc],
    filter_get_extension_integer(Tail,IntClause,NewAcc).

%% @hidden
filter_has_extension([], _, Acc) ->
    % non-reverseal is intentional.
    Acc;
filter_has_extension([{Msg,_,disallowed}|Tail], Clause, Acc) ->
    filter_has_extension(Tail, Clause, Acc);
filter_has_extension([{MsgName,_,Extends}|Tail], Clause, Acc) ->
    {clause,L,[OldRecArg,_],G,[Body]} = Clause,
		{call, L1, {remote,L1,Dict,IsKey},[_Key,DictArg]} = Body,
    RecArg = replace_atom(OldRecArg,pikachu,atomize(MsgName)),
    Folder = fun({ID, Rules, Type, Name, Other}, FoldAcc) ->
        AtomClause = {clause,L,[RecArg,{atom,L,atomize(Name)}],G,[
            {call,L,{remote,L,Dict,IsKey},[{atom,L,atomize(Name)},DictArg]}
        ]},
        IntClause = {clause,L,[RecArg,{integer,L,ID}],G,[
            {call,L,{remote,L,Dict,IsKey},[{integer,L,ID},DictArg]}
        ]},
        [AtomClause,IntClause|FoldAcc]
    end,
    NewClauses = lists:foldl(Folder, [], Extends),
    NewAcc = lists:append(Acc,NewClauses),
    filter_has_extension(Tail,Clause,NewAcc).

%% @hidden
filter_extension_size([], _RecClause, Acc) ->
    % the non-reversal is intentional.
    Acc;
filter_extension_size([{MsgName,_,disallowed}|Tail],Clause,Acc) ->
    filter_extension_size(Tail,Clause,Acc);
filter_extension_size([{MsgName,_,_}|Tail],Clause,Acc) ->
    {clause,L,[OldArg],G,Body} = Clause,
    NewClause = {clause,L,[replace_atom(OldArg,pikachu,atomize(MsgName))],G,Body},
    NewAcc = [NewClause | Acc],
    filter_extension_size(Tail,Clause,NewAcc).

%% @hidden
filter_encode_clause({MsgName, _Fields,_Extends}, {clause,L,_Args,Guards,Content}) ->
    ToIolist = {cons, L,
                {call,L, {atom,L,iolist}, [{atom,L,atomize(MsgName)},{var,L,'Record'}]},
                {call,L, {atom,L,encode_extensions}, [{var,L,'Record'}]}
               },
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'Record'}],Guards,[ToIolist]}.

expand_iolist_function(Msgs, Line, Clause) ->
    {function,Line,iolist,2,[filter_iolist_clause(Msg, Clause) || Msg <- Msgs]}.

filter_iolist_clause({MsgName, [], _Extends0}, {clause,L,Args,Guards,_Content}) ->
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'_Record'}],Guards,[{nil,L}]};
filter_iolist_clause({MsgName, Fields0, _Extends0}, {clause,L,_Args,Guards,_Content}) ->
    Fields = [
        case Tag of
        optional ->
            Field;
        _ ->
            {FNum,Tag,SType,SName,none}
        end
        || {FNum,Tag,SType,SName,_} = Field <- Fields0 ],
    Cons = lists:foldl(
	     fun({FNum,Tag,SType,SName,Default}, Acc) ->
		     {cons,L,
		      {call,L,{atom,L,pack},[{integer,L,FNum},
					     {atom,L,Tag},
					     {call,L,
					      {atom,L,with_default},
					      [{record_field,L,
						{var,L,'Record'},atomize(MsgName),
						{atom,L,atomize(SName)}},
					       erl_parse:abstract(Default)]},
					     {atom,L,atomize(SType)},
					     {nil,L}]},
		      Acc}
	     end, {nil,L}, Fields),
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'Record'}],Guards,[Cons]}.

%% @hidden
expand_decode_function(Msgs, Line, Clause) ->
    {function,Line,decode,2, [{clause,Line,[{atom,Line,enummsg_values},{integer,Line,1}],[],[{atom,Line,value1}]}] ++
     [filter_decode_clause(Msgs, Msg, Clause) || Msg <- Msgs]}.

%% @hidden
filter_decode_clause(Msgs, {MsgName, Fields, Extends}, {clause,L,_Args,Guards,[_,_,C,D]}) ->
    Types = lists:keysort(1, [begin
            {FNum, list_to_atom(SName),
			       atomize(SType),
			       decode_opts(Msgs, Tag, SType), Def} end ||
				 {FNum,Tag,SType,SName,Def} <- Fields]),
    Cons = lists:foldl(
	     fun({FNum, FName, Type, Opts, _Def}, Acc) ->
             {cons,L,{tuple,L,[{integer,L,FNum},{atom,L,list_to_atom(string:to_lower(atom_to_list(FName)))},{atom,L,Type},erl_parse:abstract(Opts)]},Acc}
	     end, {nil,L}, Types),
    ExtendDefault = case Extends of
        disallowed -> {nil,L};
        _ -> erl_parse:abstract([{false, '$extensions', dict:new()}])
    end,
    Defaults = lists:foldr(
        fun
            ({_FNum, _FName, _Type, _Opts, none}, Acc) ->
                Acc;
            ({FNum, FName, _Type, _Opts, Def}, Acc) ->
                {cons,L,{tuple,L,[{integer,L,FNum},{atom,L,list_to_atom(string:to_lower(atom_to_list(FName)))},erl_parse:abstract(Def)]},Acc}
        end,
        ExtendDefault,
        Types),
    A = {match,L,{var,L,'Types'},Cons},
    B = {match,L,{var,L,'Defaults'},Defaults},
    D1 = replace_atom(D, pikachu, atomize(MsgName)),
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'Bytes'}],Guards,[A,B,C,D1]}.

%% @hidden
filter_decode_extensions_clause(_,[],_,Acc) ->
    lists:reverse(Acc);
filter_decode_extensions_clause(Msgs,[{_,_,disallowed}|Tail],Clause,Acc) ->
    filter_decode_extensions_clause(Msgs,Tail,Clause,Acc);
filter_decode_extensions_clause(Msgs,[{MsgName,_,Extends}|Tail],Clause,Acc) ->
    {clause,L,_,_,_} = Clause,
    Types = lists:keysort(1, [{FNum, list_to_atom(SName),
			       atomize(SType),
			       decode_opts(Msgs, Tag, SType), Def} ||
				 {FNum,Tag,SType,SName,Def} <- Extends]),
    Cons = lists:foldl(
	     fun({FNum, FName, Type, Opts, _Def}, Acc) ->
		     {cons,L,{tuple,L,[{integer,L,FNum},{atom,L,FName},{atom,L,Type},erl_parse:abstract(Opts)]},Acc}
	     end, {nil,L}, Types),
    A = {match,L,{var,L,'Types'},Cons},
		{clause,L,[Arg],Guards,[_,B,C]} = Clause,
		NewBody = [A,B,replace_atom(C,pikachu,atomize(MsgName))],
		NewClause = {clause,L,[replace_atom(Arg, pikachu, atomize(MsgName))],Guards,NewBody},
    filter_decode_extensions_clause(Msgs,Tail,Clause,[NewClause|Acc]).

%% @hidden
expand_encode_function(Msgs, Line, Clause) ->
    {function,Line,encode,2,[filter_encode_clause(Msg, Clause) || Msg <- Msgs]}.

%% @hidden
decode_opts(Msgs, Tag, Type) ->
    Opts0 = if Tag == repeated -> [repeated]; Tag == repeated_packed -> [repeated_packed]; true -> [] end,
    case lists:keymember(Type, 1, Msgs) of
        true ->
            [is_record|Opts0];
        false ->
            Opts0
    end.

%% @hidden
expand_to_record_function(Msgs, Line, Clause) ->
    {function,Line,to_record,2,[filter_to_record_clause(Msg, Clause) || Msg <- Msgs]}.

%% @hidden
filter_to_record_clause({MsgName, _, Extends}, {clause,L,[_Param1,Param2],Guards,[Fold,DecodeExtends]}) ->
    Fold1 = replace_atom(Fold, pikachu, atomize(MsgName)),
    ReturnLine = case Extends of
        disallowed ->
            {var,L,'Record1'};
        _ ->
            {ok, Tokens, _} = erl_scan:string("decode_extensions(Record1)."),
	          {ok, [Abstract]} = erl_parse:parse_exprs(Tokens),
            Abstract
    end,
    {clause,L,[{atom,L,atomize(MsgName)},Param2],Guards,[Fold1,ReturnLine]}.

%% @hidden
expand_enum_to_int_function([], Line, Clause) ->
    {function,Line,enum_to_int,2,[Clause]};
expand_enum_to_int_function(Enums, Line, Clause) ->
    {function,Line,enum_to_int,2,[filter_enum_to_int_clause(Enum, Clause) || Enum <- Enums]}.

%% @hidden
filter_enum_to_int_clause({enum,EnumTypeName,IntValue,EnumValue}, {clause,L,_Args,Guards,_}) ->
    {clause,L,[{atom,L,atomize(EnumTypeName)},{atom,L,EnumValue}],Guards,[{integer,L,IntValue}]}.

%% @hidden
expand_int_to_enum_function([], Line, Clause) ->
    {function,Line,int_to_enum,2,[Clause]};
expand_int_to_enum_function(Enums, Line, Clause) ->
    {function,Line,int_to_enum,2, [filter_int_to_enum_clause(Enum, Clause) || Enum <- Enums] ++ [Clause]}.

%% @hidden
filter_int_to_enum_clause({enum,EnumTypeName,IntValue,EnumValue}, {clause,L,_Args,Guards,_}) ->
    {clause,L,[{atom,L,atomize(EnumTypeName)},{integer,L,IntValue}],Guards,[{atom,L,EnumValue}]}.

%% @hidden
%% [{"Location",
%%   [{2,required,"string","country",none},
%%    {1,required,"string","region",none}]},
%%  {"Person",
%%   [{5,optional,"Location","location",none},
%%    {4,required,"int32","age",none},
%%    {3,required,"string","phone_number",none},
%%    {2,required,"string","address",none},
%%    {1,required,"string","name",none}]}]
collect_full_messages(Data) -> collect_full_messages(Data, #collected{}).
collect_full_messages([{message, Name, Fields} | Tail], Collected) ->
    Package = Collected#collected.package,
    ListName = resolve_list_name(Name, Package),

    FieldsOut = lists:foldl(
		  fun ({_,_,_,_,_} = Input, TmpAcc) -> [Input | TmpAcc];
		      (_, TmpAcc) -> TmpAcc
		  end, [], Fields),

    Enums = lists:foldl(
	      fun ({enum,C,D}, TmpAcc) -> [{enum, [list_to_tuple([C | LN]) || LN <- ListName], D} | TmpAcc];
		  (_, TmpAcc) -> TmpAcc
	      end, [], Fields),

    Extensions = lists:foldl(
		   fun ({extensions, From, To}, TmpAcc) -> [{From,To}|TmpAcc];
		       (_, TmpAcc) -> TmpAcc
		   end, [], Fields),

    SubMessages = lists:foldl(
		    fun ({message, C, D}, TmpAcc) -> [{message, [list_to_tuple([C | LN]) || LN <- ListName], D} | TmpAcc];
			(_, TmpAcc) -> TmpAcc
		    end, [], Fields),

    ExtendedFields = case Extensions of
        [] -> disallowed;
        _ -> []
    end,

    NewCollected = Collected#collected{
		     msg=[{[list_to_tuple(L) || L <- ListName], FieldsOut, ExtendedFields} | Collected#collected.msg],
		     extensions=[{[list_to_tuple(L) || L <- ListName],Extensions} | Collected#collected.extensions]
		    },
    collect_full_messages(Tail ++ SubMessages ++ Enums, NewCollected);
collect_full_messages([{enum, Name, Fields} | Tail], Collected) ->
    Package = Collected#collected.package,
    ListName = resolve_list_name(Name, Package),

    FieldsOut = lists:foldl(
		  fun (Field, TmpAcc) ->
			  case Field of
			      {EnumAtom, IntValue} -> [{enum,
							[type_path_to_type(LN) || LN <-ListName],
							IntValue,
							EnumAtom} | TmpAcc];
			      _ -> TmpAcc
			  end
		  end, [], Fields),

    NewCollected = Collected#collected{enum=FieldsOut++Collected#collected.enum},
    collect_full_messages(Tail, NewCollected);
collect_full_messages([{package, PackageName} | Tail], Collected) ->
    collect_full_messages(Tail, Collected#collected{package = PackageName});
collect_full_messages([{option,_,_} | Tail], Collected) ->
    collect_full_messages(Tail, Collected);
collect_full_messages([{import, _Filename} | Tail], Collected) ->
    collect_full_messages(Tail, Collected);
collect_full_messages([{extend, Name, ExtendedFields} | Tail], Collected) ->
    SeekName0 = string:tokens(Name, "."),
    SeekName1 = [list_to_tuple(lists:reverse(SeekName0))],
    SeekNames0 = resolve_list_name(SeekName1, Collected#collected.package),
    SeekNames = [list_to_tuple(SN) || SN <- SeekNames0],

    #collected{msg = CollectedMsg, extensions = Extended} = Collected,
    BestMatch = element(1, find_message_by_path(SeekNames, CollectedMsg)),

    %{ListNameT,FieldsOut,ExtendFields} = find_extended_msg(ListName, CollectedMsg),
    %{ListNameT2,Extensions} = find_defined_extensions(ListName, Extended),
    {ListName,FieldsOut,ExtendFields} = lists:keyfind(BestMatch,1,CollectedMsg),
    {ListName,Extensions} = lists:keyfind(BestMatch,1,Collected#collected.extensions),

    FunNotInReservedRange = fun(Id) -> not(19000 =< Id andalso Id =< 19999) end,
    FunInRange = fun(Id,From,max) -> From =< Id andalso Id =< 16#1fffffff;
		    (Id,From,To) -> From =< Id andalso Id =< To
		 end,

    ExtendedFieldsOut = lists:append(FieldsOut,
			     lists:foldl(
			       fun ({Id, _, _, FieldName, _} = Input, TmpAcc) ->
				       case lists:any(fun({From,To}) -> FunNotInReservedRange(Id)
									    andalso FunInRange(Id,From,To)
						      end,Extensions) of
					   true ->
					       [Input | TmpAcc];
					   _ ->
					       error_logger:error_report(["Extended field not in valid range",
									  {message, Name},
									  {field_id,Id},
									  {field_name,FieldName},
									  {defined_ranges,Extensions},
									  {reserved_range,{19000,19999}},
									  {max,16#1fffffff}]),
					       throw(out_of_range)
				       end;
				   (_, TmpAcc) -> TmpAcc
			       end, [], ExtendedFields)
			     ),
    NewExtends = case ExtendFields of
        disallowed -> disallowed;
        _ -> ExtendFields ++ ExtendedFieldsOut
    end,
    NewCollected = Collected#collected{msg=lists:keyreplace(ListName,1,CollectedMsg,{ListName,FieldsOut,NewExtends})},
    collect_full_messages(Tail, NewCollected);
collect_full_messages([file_boundary | Tail], Collected) ->
    collect_full_messages(Tail, Collected#collected{package = undefined});
%% Skip anything we don't understand
collect_full_messages([Skip|Tail], Acc) ->
    error_logger:warning_report(["Unkown, skipping",
				 {skip,Skip}]),
    collect_full_messages(Tail, Acc);
collect_full_messages([], Collected) ->
    Collected.

%% @hidden
find_message_by_path(_TypeName, []) ->
    false;
find_message_by_path(TypeName, [Msg | Tail]) ->
    Names = element(1, Msg),
    case [N || N <- Names, lists:member(N, TypeName)] of
        [] ->
            find_message_by_path(TypeName, Tail);
        _ ->
            Msg
    end.

%% @hidden
find_extended_msg(_TypeName, []) ->
    false;
find_extended_msg(TypeName, Msgs) when is_list(hd(TypeName)) ->
    TypeName0 = [list_to_tuple(TN) || TN <- TypeName],
    find_extended_msg(TypeName0, Msgs);
find_extended_msg(TypeName, [Msg | Tail]) ->
    Names = element(1, Msg),
    case [N || N <- Names, lists:member(N, TypeName)] of
        [] ->
            find_extended_msg(TypeName, Tail);
        _ ->
            Msg
    end.

%% @hidden
find_defined_extensions(TypeName, []) ->
    false;
find_defined_extensions(TypeName, Extends) when is_list(hd(TypeName)) ->
    TypeName0 = [list_to_tuple(TN) || TN <- TypeName],
    find_defined_extensions(TypeName0, Extends);
find_defined_extensions(TypeName, [Ext | Tail]) ->
    Names = element(1, Ext),
    case [N || N <- Names] of
        [] ->
            find_defined_extensions(TypeName, Tail);
        _ ->
            Ext
    end.

%% @hidden
resolve_list_name(Name, _Package) when is_tuple(hd(Name)) ->
    [tuple_to_list(N) || N <- Name];
resolve_list_name(Name, undefined) when is_integer(hd(Name)) ->
    [[Name]];
resolve_list_name(Name, Package) when is_integer(hd(Name)) ->
    [[Name], [Name, Package]].

%% @hidden
resolve_types(Data, Enums) -> resolve_types (Data, Data, Enums, []).
resolve_types([{TypePath, Fields,Extended} | Tail], AllPaths, Enums, Acc) ->
    FolderFun = fun (Input, TmpAcc) ->
			  case Input of
			      {Index, Rules, Type, Identifier, Other} ->
				  case is_scalar_type(Type) of
				      true -> [Input | TmpAcc];
				      false ->
					  PossiblePaths =
					      case string:tokens(Type,".") of
						  [Type] ->
						      all_possible_type_paths(Type, TypePath);
						  FullPath ->
						% handle types of the form Foo.Bar which are absolute,
						% so we just convert to a type path and check it.
						      [lists:reverse(FullPath)]
					      end,
					  RealPath =
					      case find_type(PossiblePaths, AllPaths) of
						  false ->
						      case is_enum_type(Type, PossiblePaths, Enums) of
							  {true,EnumType} ->
							      [EnumType];
							  false ->
							      throw(["Unknown Type ", Type])
						      end;
						  ResultType ->
						      ResultType
					      end,
					  [{Index, Rules, canonical_name(RealPath), Identifier, Other} | TmpAcc]
				  end;
			      _ -> TmpAcc
			  end
		end,
    FieldsOut = lists:foldl(FolderFun, [], Fields),
    ExtendedOut = case Extended of
        disallowed ->
            disallowed;
        _ ->
            MidExtendOut = lists:foldl(FolderFun, [], Extended),
            lists:reverse(MidExtendOut)
    end,
    Type = type_path_to_type(TypePath),
    resolve_types(Tail, AllPaths, Enums, [{Type, lists:reverse(FieldsOut), ExtendedOut } | Acc]);
resolve_types([], _, _, Acc) ->
    Acc.

%% @hidden
write_header_include_file(Basename, Messages) when is_list(Basename) ->
    {ok, FileRef} = protobuffs_file:open(Basename, [write]),
    write_header_include_file(FileRef, Messages),
    protobuffs_file:close(FileRef);
write_header_include_file(_FileRef, []) ->
    ok;
    write_header_include_file(FileRef, [{Name, Fields, Extends} | Tail]) ->
    OutFields = [{string:to_lower(A), Optional, Default} || {_, Optional, _, A, Default} <- lists:keysort(1, Fields)],
    DefName = string:to_upper(Name) ++ "_PB_H",
    protobuffs_file:format(FileRef, "-ifndef(~s).~n-define(~s, true).~n", [DefName, DefName]),
    protobuffs_file:format(FileRef, "-record(~s, {~n    ", [string:to_lower(Name)]),
    WriteFields0 = generate_field_definitions(OutFields),
    WriteFields = case Extends of
                      disallowed -> WriteFields0;
                      _ ->
                          ExtenStr = "'$extensions' = dict:new()",
                          WriteFields0 ++ [ExtenStr]
                  end,
    FormatString = string:join(["~s" || _ <- lists:seq(1, length(WriteFields))], ",~n    "),
    protobuffs_file:format(FileRef, FormatString, WriteFields),
    protobuffs_file:format(FileRef, "~n}).~n", []),
    protobuffs_file:format(FileRef, "-endif.~n~n", []),
    write_header_include_file(FileRef, Tail).

%% @hidden
generate_field_definitions(Fields) ->
    generate_field_definitions(Fields, []).

%% @hidden
generate_field_definitions([], Acc) ->
    lists:reverse(Acc);
generate_field_definitions([{Name, required, _} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s = erlang:error({required, ~s})", [Name, Name])),
    generate_field_definitions(Tail, [Head | Acc]);
generate_field_definitions([{Name, _, none} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s", [Name])),
    generate_field_definitions(Tail, [Head | Acc]);
generate_field_definitions([{Name, _, Default} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s = ~p", [Name, Default])),
    generate_field_definitions(Tail, [Head | Acc]).

%% @hidden
atomize(String) ->
    list_to_atom(string:to_lower(String)).

%% @hidden
replace_atom(Find, Find, Replace) -> Replace;
replace_atom(Tuple, Find, Replace) when is_tuple(Tuple) ->
    list_to_tuple([replace_atom(Term, Find, Replace) || Term <- tuple_to_list(Tuple)]);
replace_atom(List, Find, Replace) when is_list(List) ->
    [replace_atom(Term, Find, Replace) || Term <- List];
replace_atom(Other, _Find, _Replace) ->
    Other.

%% @hidden
is_scalar_type("double") -> true;
is_scalar_type("float") -> true;
is_scalar_type("int32") -> true;
is_scalar_type("int64") -> true;
is_scalar_type("uint32") -> true;
is_scalar_type("uint64") -> true;
is_scalar_type("sint32") -> true;
is_scalar_type("sint64") -> true;
is_scalar_type("fixed32") -> true;
is_scalar_type("fixed64") -> true;
is_scalar_type("sfixed32") -> true;
is_scalar_type("sfixed64") -> true;
is_scalar_type("bool") -> true;
is_scalar_type("string") -> true;
is_scalar_type("bytes") -> true;
is_scalar_type(_) -> false.

%% @hidden
is_enum_type(_Type, [], _Enums) ->
    false;
is_enum_type(Type, [TypePath|Paths], Enums) ->
    TypeFromPath = type_path_to_type(TypePath),
    case is_enum_type(TypeFromPath, Enums) of
      true ->
            {true,TypePath};
      false ->
            is_enum_type(Type, Paths, Enums)
    end.
is_enum_type(Type, Enums) ->
    lists:any(fun(Enum) ->
        Types = element(2, Enum),
        lists:member(Type, Types)
    end, Enums).

%% @hidden
sublists(List) when is_list(List) ->
    sublists(List,[]).
sublists([],Acc) ->
    [ [] | Acc ];
sublists(List,Acc) ->
    sublists(tl(List), [ List | Acc ]).

%% @hidden
all_possible_type_paths(Type, TypePaths) ->
    all_possible_type_paths(Type, TypePaths, []).

all_possible_type_paths(_Type, [], Acc) ->
    lists:reverse(lists:flatten(Acc));
all_possible_type_paths(Type, [TypePath | Tail], Acc) ->
    TypePath0 = tuple_to_list(TypePath),
%    Type0 = if
%        is_list(hd(Type)) -> Type;
%        true -> [Type]
%    end,
%    FoldFun = fun(TypeSuffix, AccIn) ->
%        [[Type0 ++ TypeSuffix] | AccIn]
%    end,
%    lists:foldl(FoldFun, Type0, TypePath).
    Head = lists:foldl(fun (TypeSuffix, AccIn) ->
			 [list_to_tuple([Type | TypeSuffix]) | AccIn]
		 end,
		 [],
		 sublists(TypePath0)),
    all_possible_type_paths(Type, Tail, [Head | Acc]).

%% @hidden
find_type([], _KnownTypes) ->
    false;
find_type([Type | TailTypes], KnownTypes) when is_list(Type) ->
    find_type([list_to_tuple(Type) | TailTypes], KnownTypes);
find_type([Type | TailTypes], KnownTypes) ->
    FilterFun = fun(KnownType) ->
        lists:member(Type, element(1, KnownType))
    end,
    case lists:filter(FilterFun, KnownTypes) of
        [] ->
            find_type(TailTypes, KnownTypes);
        [{RealType, _, _} | _] ->
            RealType
    end.

%% @hidden
canonical_name([TypePath | _]) ->
    % yes, we're assuming the first name in the list is the most
    % cannonical name.
    type_path_to_type(TypePath).

canonize_names(Messages) ->
    canonize_names(Messages, []).

canonize_names([], Acc) ->
    lists:reverse(Acc);
canonize_names([Enum | Tail], Acc) when element(1, Enum) == enum ->
    Name = canonical_name(element(2, Enum)),
    Enum0 = setelement(2, Enum, Name),
    canonize_names(Tail, [Enum0 | Acc]);
canonize_names([Msg | Tail], Acc) ->
    Name = canonical_name(element(1, Msg)),
    Msg0 = setelement(1,Msg,Name),
    canonize_names(Tail, [Msg0 | Acc]).

%% @hidden
type_path_to_type([[Name|Tuple]]) when is_tuple(Tuple) ->
    type_path_to_type([Name | tuple_to_list(Tuple)]);
type_path_to_type(TypePath) when is_tuple(TypePath) ->
    type_path_to_type(tuple_to_list(TypePath));
type_path_to_type(TypePath) ->
    case is_list(hd(TypePath)) of
        true ->
            string:join(lists:reverse(TypePath), "_");
        false ->
            TypePath
    end.
