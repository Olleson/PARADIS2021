-module(books).
-behaviour(gen_server).
-export([start/0, add_user/1, add_book/2, get_books/1, lend/3]).
-record(state, {books = #{}}).

%%% Callbacks
start_link() -> gen_server:start_link({local, books}, ?MODULE, [], []).

init(_) -> {ok, #state{}}.

handle_call({add_user, User}, _From, State = #state{books = Books}) ->
    {Reply, New_books} = 
    case maps:find(User, Books) of
        error ->
            {ok, Books#{User => []}};
        _ ->
            {already_exists, Books}
        end,
    {reply, Reply, State#state{books = New_books}};
handle_call({add_book, User, Book}, _From, State = #state{books = Books}) ->
    {Reply, New_books} =
    case maps:find(User, Books) of
        error -> 
            {no_user, Books};
        {ok, List} ->
            {ok, Books#{User => [Book|List]}}   % currently does not check if user already owns book or not
    end,
    {reply, Reply, State#state{books = New_books}};
handle_call({get_books, User}, _From, State = #state{books = Books}) ->
    Reply =
    case maps:find(User, Books) of 
        error ->
            no_user;
        {ok, List} ->
            {ok, List}
    end,
    {reply, Reply, State};

% I could've used a tuple list for the books instead of a list to use functions like lists:keyfind() but I'm out of time, lol.
handle_call({lend, UserA, UserB, Book}, _From, State = #state{books = Books}) ->
    ListA = maps:find(UserA, Books),
    ListB = maps:find(UserB, Books),
    {Reply, New_books} =
    case {ListA, ListB} of
        {error, _} ->
            {no_user, Books};
        {_, error} -> 
            {no_user, Books};
        {[], _} ->
            {no_book, Books};
        {{ok, A}, {ok, B}} ->
            New_listA = [Y || Y <- A, Y =/= Book],          % Make new list without the book that is lent out.
            New_listB = [Z || Z <- A, Z == Book],           % Only take books out that match with book that is being borrowed.
            {ok, Books#{UserB => [hd(New_listB)|B], UserA => New_listA}}    % Get head of new list
    end,
    {reply, Reply, State#state{books = New_books}}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

%%% Interface
start() -> start_link().

add_user(User) -> 
    gen_server:call(books, {add_user, User}).

add_book(User, Book) ->
    gen_server:call(books, {add_book, User, Book}).

get_books(User) ->
    gen_server:call(books, {get_books, User}).

lend(UserA, UserB, Book) ->
    gen_server:call(books, {lend, UserA, UserB, Book}).