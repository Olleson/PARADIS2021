% Gen_server implementation of bank application
-module(bank).
-behaviour(gen_server).
-export([start/0, terminate/2, stop/1,
         balance/2, deposit/3, withdraw/3, lend/4]).

-record(state, {bank = #{}}).

%%% Server functions
start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []), Pid.
    % gen_server:start_link(?MODULE, [], []). 

init(_) ->
    {ok, #state{}}.

% Synchronous calls
handle_call({balance, User}, _From, State = #state{bank = Bank}) ->
    Response = case maps:find(User, Bank) of
        error ->
            no_account;
        {ok, Balance} ->
            {ok, Balance}
        end,
    {reply, Response, State};
handle_call({deposit, {User, X}}, _From, State = #state{bank = Bank}) ->
    {Response, New_bank} = case maps:find(User, Bank) of
        error -> 
            {{ok, X}, Bank#{User => X}};
        {ok, Balance} ->
            New_amount = Balance + X,
            {{ok, New_amount}, Bank#{User => New_amount}}
        end,
    {reply, Response, State#state{bank = New_bank}};
handle_call({withdraw, {User, X}}, _From, State = #state{bank = Bank}) ->
    {Response, New_bank} = case maps:find(User, Bank) of
        error ->
            {no_account, Bank};
        {ok, Balance} when X > Balance ->
            {insufficient_funds, Bank};
        {ok, Balance} ->
            New_amount = Balance - X,
            {{ok, New_amount}, Bank#{User => New_amount}}
        end,
    {reply, Response, State#state{bank = New_bank}}; 
handle_call({lend, {From, To, X}}, _From, State = #state{bank = Bank}) ->
    Balance1 = maps:get(From, Bank, error),
    Balance2 = maps:get(To, Bank, error),
    {Response, New_bank} = case {Balance1, Balance2} of 
        {error, error} ->
            {{no_account, both}, Bank};
        {error, _} ->
            {{no_account, From}, Bank};
        {_, error} ->
            {{no_account, To}, Bank};
        {Balance1, Balance2} when Balance1 >= X ->
            {{ok}, Bank#{From => Balance1 - X, To => Balance2 + X}};
        {Balance1, Balance2} ->
            {{insufficient_funds}, Bank}
        end,
    {reply, Response, State#state{bank = New_bank}, 1000}.

handle_cast(_, _) ->
    ok.

% handle_info(timeout, State) ->
%     no_bank,
%     io:format("timed out"),
%     {noreply, State};
handle_info(Info, State) -> 
    io:format("got ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%% Interface functions
balance(Pid, Who) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {balance, Who})
    catch
        exit:_ -> no_bank
    end.

deposit(Pid, Who, X) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {deposit, {Who, X}})
    catch
        exit:_ -> no_bank
    end.

withdraw(Pid, Who, X) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {withdraw, {Who, X}})
    catch
        exit:_ -> no_bank
    end.

lend(Pid, From, To, X) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {lend, {From, To, X}})
    catch
        exit:_ -> no_bank
    end.

stop(Pid) when is_pid(Pid) ->
    gen_server:stop(Pid).

% balance(Pid, Who) when is_pid(Pid) ->
%     gen_server:call(Pid, {balance, Who}).

% deposit(Pid, Who, X) when is_pid(Pid) ->
%     gen_server:call(Pid, {deposit, {Who, X}}).

% withdraw(Pid, Who, X) when is_pid(Pid) ->
%     gen_server:call(Pid, {withdraw, {Who, X}}).

% lend(Pid, From, To, X) when is_pid(Pid) ->
%     gen_server:call(Pid, {lend, {From, To, X}}).

% stop(Pid) when is_pid(Pid) ->
%     gen_server:stop(Pid).

