%%
%% Description: Crontab in Erlang.
%% Copyright (c) 2014 ShionRyuu <shionryuu@outlook.com>.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
-module(crontab_server).
-author("ShionRyuu").

-behaviour(gen_server).

-include("crontab.hrl").

%% API
-export([
    start_link/0,
    stop/0,
    add_crontab/4,
    del_crontab/2,
    info/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    ref,
    n2e = dict:new(), %% name -> entry
    p2n = dict:new(), %% pid -> name, dict or list ?
    n2p = dict:new()  %% name -> pid
}).

%% ============================================================================
%% API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

%% add a crontab
add_crontab(Name, Time, MFA, Options) ->
    gen_server:call(?SERVER, {add_crontab, Name, Time, MFA, Options}).

%% remove a crontab
del_crontab(Name, Options) ->
    gen_server:call(?SERVER, {del_crontab, Name, Options}).

info() ->
    gen_server:call(?SERVER, info).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, TRef} = timer:send_after(get_next_tick(), tick),
    {ok, #state{ref = TRef}}.

handle_call({add_crontab, Name, Time, MFA, Options}, _From, State) ->
    case do_add_crontab(State#state.n2e, Name, Time, MFA, Options) of
        {ok, N2E} -> {reply, ok, State#state{n2e = N2E}};
        Err -> {reply, Err, State}
    end;

handle_call({del_crontab, Name, Options}, _From, State) ->
    case do_del_crontab(State#state.n2e, State#state.n2p, Name, Options) of
        {ok, N2E, N2P} -> {reply, ok, State#state{n2e = N2E, n2p = N2P}};
        Err -> {reply, Err, State}
    end;

handle_call(info, _From, State) ->
    List = [Entry || Entry <- dict:to_list(State#state.n2e)],
    ActiveList = [Name || Name <- dict:to_list(State#state.n2p)],
    Reply = {
        {entry_size, length(List)},
        {entry_list, List},
        {active_size, length(ActiveList)},
        {active_list, ActiveList}
    },
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    {N2P, P2N} = tick(State#state.n2e, State#state.n2p, State#state.p2n),
    {ok, TRef} = timer:send_after(get_next_tick(), tick),
    {noreply, State#state{n2p = N2P, p2n = P2N, ref = TRef}};

handle_info({'EXIT', Pid, Rsn}, State) ->
    {N2E, N2P, P2N} = handle_exit(Pid, Rsn, State#state.n2e, State#state.n2p, State#state.p2n),
    {noreply, State#state{p2n = P2N, n2p = N2P, n2e = N2E}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% Internal functions
%% ============================================================================

%%
do_add_crontab(N2E, Name, Time, MFA, Options) ->
    case dict:is_key(Name, N2E) of
        true -> {error, existing};
        _ ->
            case crontab_parser:parse_entry(Name, Time, MFA, Options) of
                {ok, Entry} ->
                    {ok, dict:store(Name, Entry, N2E)};
                Err -> Err
            end
    end.

%%
do_del_crontab(N2E0, N2P0, Name, Options) ->
    case dict:is_key(Name, N2P0) of
        true ->
            {_, Pid} = dict:fetch(N2P0, Name),
            {_, Entry} = dict:fetch(N2E0, Name),
            try_stop(Entry, Pid, Options),
            {ok, safe_del(Name, N2E0), safe_del(Name, N2P0)};
        _ ->
            {error, notfound}
    end.

try_stop(Entry, Pid, Options) ->
    #entry{options = Opts} = Entry,
    case proplists:get_value(stop_on_del, Options ++ Opts, false) of
        true -> erlang:exit(Pid, remove);
        _ -> ignore
    end.

%%
safe_del(Key, Dict) ->
    case dict:is_key(Key, Dict) of
        true -> dict:erase(Key, Dict);
        _ -> Dict
    end.

%%
tick(N2E0, N2P0, P2N0) ->
    {Date, {Hour0, Min0, _}} = erlang:localtime(),
    {_, Mon0, Dom0} = Date,
    Dow0 = calendar:day_of_the_week(Date), %% 7 for Sunday
    lists:foldl(fun({_, Entry}, {N2PD, P2ND}) ->
        case catch try_start(Entry, N2PD, Min0, Hour0, Dom0, Mon0, Dow0) of
            {ok, Pid} ->
                #entry{name = Name} = Entry,
                {dict:store(Name, Pid, N2PD), dict:store(Pid, Name, P2ND)};
            _ -> {N2PD, P2ND}
        end
    end, {N2P0, P2N0}, dict:to_list(N2E0)).

%%
try_start(Entry, N2PD, Min0, Hour0, Dom0, Mon0, Dow0) ->
    #entry{name = Name} = Entry,
    case (not dict:is_key(Name, N2PD)) %%
        andalso should_start(Entry, Min0, Hour0, Dom0, Mon0, Dow0) of
        true ->
            #entry{mfa = {M, F, A}} = Entry,
            {ok, erlang:spawn_link(M, F, A)};
        _ -> ignore
    end.

should_start(Entry, Min0, Hour0, Dom0, Mon0, Dow0) ->
    #entry{
        min = Minute, hour = Hour, month = Month,
        dom = Dom, dow = Dow, flags = Flags} = Entry,
    bit_test(Minute, Min0) andalso
        bit_test(Hour, Hour0) andalso
        bit_test(Month, Mon0) andalso
        ?IF((flag_set(Flags, ?DOM_STAR) orelse flag_set(Flags, ?DOW_STAR)),
            (bit_test(Dow, Dow0) andalso bit_test(Dom, Dom0)),
            (bit_test(Dow, Dow0) orelse bit_test(Dom, Dom0))).

bit_test(Bits, I) ->
    bitstring:is_set(Bits, I).

flag_set(Flags, F) ->
    Flags band F > 0.

%%
handle_exit(Pid, Rsn, N2E0, N2P0, P2N0) ->
    Name = dict:fetch(Pid, P2N0),
    N2E =
        case Rsn of
            normal -> N2E0;
            _ ->
                error_logger:error_msg("crontab ~w stop due to ~w", [Name, Rsn]),
                safe_del(Name, N2E0)
        end,
    N2P = safe_del(Name, N2P0),
    P2N = safe_del(Pid, P2N0),
    {N2E, N2P, P2N}.


get_next_tick() ->
    Now = timestamp(),
    Rem = Now rem 60,
%%     ?IF(Rem =:= 0, 0, (60 - Rem) * 1000).
    (60 - Rem) * 1000.

timestamp() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.
