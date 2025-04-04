-module(battle_rank_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

start() ->
    {ok,_} =
        supervisor:start_child(world_sup, 
                               {?MODULE,
                                {?MODULE, start_link, []},
                                permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_rank_info(RoleID, DungeonID, Type) ->
    erlang:send(?MODULE, {new_rank_info, RoleID, DungeonID, Type}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {rank_list=[]}).

-define(DUMP_INTERVAL, (1000 * 60 * 10)).

-define(RANK_NUM, 50).

-record(rank_data, {roleID=0,dungeonID=0,timestamp=0,rank=0}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    process_flag(trap_exit,true),
    case db_sql:get_etc(?DB_ETC_KEY_BATTLE_RANK) of
        [{state,State}|AllInfo] ->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, AllInfo);
        _ ->
            State = #state{}
    end,
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    {ok, State}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, State) ->
    do_persist(State),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================


do_persist(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_BATTLE_RANK,Info2).

is_persist(_) ->
    false.

do_handle_info({new_rank_info, RoleID, DungeonID, Type}, State) ->
    {ok, NewState} = new_rank_info(RoleID, DungeonID, Type, State),
    {noreply, NewState};
do_handle_info({client_msg, RoleID, #cs_rank_info{type=Type,start=Start,num=Num}}, State) ->
    get_battle_rank(RoleID, Type, Start, Num, State),
    {noreply, State};
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
    {noreply, State};
do_handle_info(Info, State) ->
    ?ERR("unkown info:~w", [Info]),
    {noreply, State}.


new_rank_info(RoleID, DungeonID, Type, #state{rank_list=RankList}=State) ->
    Now = util:now(),
    case lists:keyfind(Type, 1, RankList) of
        false ->
            {ok, NewTypeRankList} = new_rank_info2(RoleID, DungeonID, Now, []),
            NewRankList = [{Type, NewTypeRankList}|RankList];
        {Type, TypeRankList} ->
            {ok, NewTypeRankList} = new_rank_info2(RoleID, DungeonID, Now, TypeRankList),
            NewRankList = lists:keyreplace(Type, 1, RankList, {Type, NewTypeRankList})
    end,
    {ok, State#state{rank_list=NewRankList}}.

new_rank_info2(RoleID, DungeonID, Now, List) ->
    NewRankData = #rank_data{roleID=RoleID,dungeonID=DungeonID,timestamp=Now},
    case lists:keyfind(RoleID, #rank_data.roleID, List) of
        false ->
            case erlang:length(List) >= ?RANK_NUM of
                true ->
                    #rank_data{dungeonID=LastDungeonID} = LastRankData = lists:last(List),
                    case DungeonID > LastDungeonID of
                        true ->
                            NewList = [NewRankData|lists:delete(LastRankData, List)];
                        false ->
                            NewList = List
                    end;
                false ->
                    NewList = [NewRankData|List]
            end;
        _ ->
            NewList = lists:keyreplace(RoleID, #rank_data.roleID, List, NewRankData)
    end,
    case List =/= NewList of
        true ->
            {ok, sort_rank_list(NewList)};
        false ->
            {ok, List}
    end.

sort_rank_list(List) ->
    NewList =
        lists:sort(
          fun(#rank_data{dungeonID=DungeonID1,timestamp=Timestamp1}, #rank_data{dungeonID=DungeonID2,timestamp=Timestamp2}) ->
                  if
                      DungeonID1 > DungeonID2 ->
                          true;
                      DungeonID1 =:= DungeonID2 ->
                          if
                              Timestamp1 < Timestamp2 ->
                                  true;
                              true ->
                                  false
                          end;
                      true ->
                          false
                  end
          end, List),
    {NewList2, _} =
        lists:foldr(fun(RankData, {AccList, AccRank}) ->
                            {[RankData#rank_data{rank=AccRank}|AccList], AccRank - 1}
                    end, {[], erlang:length(NewList)}, NewList),
    NewList2.

get_battle_rank(RoleID, Type, Start, Num, #state{rank_list=RankList}) ->
    case lists:keyfind(Type, 1, RankList) of
        false ->
            ?unicast(RoleID, #sc_rank_info{rankType=?RANK_TYPE_BATTLE, type=Type, list=[]});
        {Type, TypeRankList} ->
            ?unicast(RoleID, #sc_rank_info{rankType=?RANK_TYPE_BATTLE, type=Type, list=trans_p_battle_rank(lists:sublist(TypeRankList, erlang:max(Start, 1), erlang:min(Num, 50)))})
    end.

trans_p_battle_rank(List) ->
    lists:map(fun(#rank_data{roleID=RoleID,dungeonID=DungeonID,rank=Rank}) ->
                      #rolePublic{
                                  roleName=RoleName,
                                  level=Level,
                                  isMale=IsMale,
                                  title=Title,
                                  fightPower=FightPower,
                                  head=Head
                                 } = role_lib:get_rolePublic(RoleID),
                      #data_dungeon{dungeonName=DungeonName,chapterID=ChapterID} = data_dungeon:get(DungeonID),
                      #p_rank_info{
                                     rank=Rank
                                     ,roleID=RoleID
                                     ,level=Level
                                     ,name=RoleName
                                     ,head=Head
                                     ,title=Title
                                     ,isMale=IsMale
                                     ,fightPower=FightPower
                                     ,chapterID=ChapterID
                                     ,dungeonName=DungeonName
                                    }
              end, List).







