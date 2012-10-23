%%%-------------------------------------------------------------------
%%% File    : pb_gs_done.erl
%%% Author  : Ken Friis Larsen <kflarsen@diku.dk>
%%% Description : Phonebook server implemented using gen_server
%%%
%%% Created : Oct 2011 by Ken Friis Larsen <kflarsen@diku.dk>
%%%-------------------------------------------------------------------
-module(pb_gs_done).

-behaviour(gen_server).

%% API
-export([start/0, stop/0,add/1,list_all/0,update/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

add(Contact)    -> gen_server:call(?MODULE, {add, Contact}).
list_all()      -> gen_server:call(?MODULE, list_all).
update(Contact) -> gen_server:call(?MODULE, {update, Contact}).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add, {Name, _, _} = Contact}, _From, Contacts) ->
    case dict:is_key(Name, Contacts) of 
        false -> {reply, ok, dict:store(Name, Contact, Contacts)};
        true  -> throw({Name, is_already_there})
    end;
handle_call(list_all, _From, Contacts) ->
    List = dict:to_list(Contacts),
    {reply, {ok, lists:map(fun({_, C}) -> C end, List)},
     Contacts};
handle_call({update, {Name, _, _} = Contact}, _From, Contacts) ->
    {reply, ok, dict:store(Name, Contact, Contacts)};
handle_call(stop, _From, Contacts) ->
    {stop, user_request, stopped, Contacts}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
