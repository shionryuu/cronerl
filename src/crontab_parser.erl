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
-module(crontab_parser).
-author("ShionRyuu").

-include("crontab.hrl").

%% API
-export([
    parse_file/1,
    parse_entrys/1,
    parse_entry/4
]).


%% parse the crontab config file
parse_file(File) ->
    case file:consult(File) of
        {error, enoent} = Error ->
            error_logger:info_msg("crontab file ~p not exist", [File]),
            Error;
        {error, R} = Error ->
            error_logger:info_msg("crontab file error: ~p", [file:format_error(R)]),
            Error;
        {ok, CronTab} ->
            parse_entrys(CronTab)
    end.

%% parse all the entrys
parse_entrys(CronTab) ->
    Entrys = lists:foldl(fun(Entry, Acc) ->
        case parse_entry(Entry) of
            {ok, CronEntry} ->
                [CronEntry | Acc];
            {error, R} ->
                error_logger:info_msg("crontab parsing entry:~p ~nerror:~p", [Entry, R]),
                Acc
        end
    end, [], CronTab),
    {ok, Entrys}.

parse_entry({Name, Time, MFA}) ->
    parse_entry(Name, Time, MFA, []);
parse_entry({Name, Time, MFA, Options}) ->
    parse_entry(Name, Time, MFA, Options);
parse_entry(_) ->
    {error, ?ERR_FMT}.

%%
parse_entry(Name, Time, MFA, Options) ->
    case catch parse_entry_t(Name, Time, MFA, Options) of
        #entry{} = V -> {ok, V};
        {error, Error} -> {error, Error}
    end.

%%
parse_entry_t(Name, Time, MFA, Options) ->
    check_time(Time),
    check_mfa(MFA),
    {MinSpec, HrsSpec, DomSpec, MonSpec, DowSpec} = Time,
    {MinFlag, MinBits} = get_spec_bits(MinSpec, ?FIRST_MIN, ?LAST_MIN, [], ?MIN_STAR, ?ERR_MIN),
    {HrsFlag, HrsBits} = get_spec_bits(HrsSpec, ?FIRST_HRS, ?LAST_HRS, [], ?HRS_STAR, ?ERR_HRS),
    {DomFlag, DomBits} = get_spec_bits(DomSpec, ?FIRST_DOM, ?LAST_DOM, [], ?DOM_STAR, ?ERR_DOM),
    {MonFlag, MonBits} = get_spec_bits(MonSpec, ?FIRST_MON, ?LAST_MON, ?MonMap, 0, ?ERR_MON),
    {DowFlag, DowBits} = get_spec_bits(DowSpec, ?FIRST_DOW, ?LAST_DOW, ?DowMap, ?DOW_STAR, ?ERR_DOW),
    %% make sundays equivilent
    SpDowBits = ?IF(bitstring:is_set(DowBits, 0) orelse bitstring:is_set(DowBits, 7),
        bitstring:list_set(DowBits, [0, 7]), DowBits),
    #entry{
        min = MinBits, hour = HrsBits, dom = DomBits, month = MonBits, dow = SpDowBits,
        name = Name, flags = MinFlag bor HrsFlag bor DomFlag bor MonFlag bor DowFlag,
        mfa = MFA, options = Options
    }.

%% check time
check_time({_Min, _Hour, _Dom, _Mon, _Dow}) ->
    ok;
check_time(_) ->
    erlang:throw({error, ?ERR_FMT}).

%% check mfa
check_mfa({M, F, A}) when is_atom(M), is_atom(F), is_list(A) ->
    ok;
check_mfa(_) ->
    erlang:throw({error, ?ERR_MFA}).

%% get wildcard flag & bits of spec
get_spec_bits(Spec, Low, High, Map, StarFlag, Err) ->
    {Star, Zones} = parse_spec(Spec, Low, High, Map, Err),
    Bits = lists:foldl(fun({Min, Max, Span}, Acc) ->
        bitstring:range_set(Acc, Min, Max, Span)
    end, 0, Zones),
    {if Star -> StarFlag;true -> 0 end, Bits}.

%%
parse_spec('*', Low, High, _Map, _Err) ->
    {true, [{Low, High, 1}]};
parse_spec(Int, Low, High, _Map, _Err) when is_integer(Int), Int >= Low, Int =< High ->
    {true, [{Int, Int, 1}]};
parse_spec(Spec, Low, High, Map, Err) when is_list(Spec) ->
    lists:foldl(fun(Zone, {StarFlag, Acc}) ->
        {Star, {Min, Max, Span}} = parse_zone(string:tokens(Zone, "-/"), Low, High, Map, Err),
        check_range(Low, High, Min, Max, Span, Err),
        {StarFlag orelse Star, [{Min, Max, Span} | Acc]}
    end, {false, []}, string:tokens(Spec, ","));
parse_spec(_Spec, _Low, _High, _Map, Err) ->
    erlang:throw({error, Err}).

%%
parse_zone(["*"], Low, High, _Map, _Err) ->
    {true, {Low, High, 1}};
parse_zone(["*", Span], Low, High, Map, _Err) ->
    {true, {Low, High, get_number(Span, Map)}};
parse_zone([Name], _Low, _High, Map, _Err) ->
    Int = get_number(Name, Map),
    {false, {Int, Int, 1}};
parse_zone([Min, Max], _Low, _High, Map, _Err) ->
    {false, {get_number(Min, Map), get_number(Max, Map), 1}};
parse_zone([Min, Max, Span], _Low, _High, Map, _Err) ->
    {false, {get_number(Min, Map), get_number(Max, Map), get_number(Span, Map)}};
parse_zone(_, _Low, _High, _Map, Err) ->
    erlang:throw({error, Err}).

%% check whether is range valid
check_range(Low, High, Min, Max, Span, _Err)
    when Min >= 0, Max >= 0, Span > 0, Max >= Min, Min >= Low, Max =< High ->
    ok;
check_range(_Low, _High, _Min, _Max, _Span, Err) ->
    erlang:throw({error, Err}).

%%
get_number(Val, Map) ->
    case string:to_integer(Val) of
        {Int, []} -> Int;
        _ -> proplists:get_value(Val, Map, -1)
    end.

%% ============================================================================
%% Tests
%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
parse_spec_fail_test() ->
    {error, ?ERR_MIN} = parse_entry(1, {"0-60", "*", '*', '*', '*'}, {io, format, ["1 triggered~n"]}, []),
    {error, ?ERR_MIN} = parse_entry(2, {"59,62,63", "*", '*', '*', '*'}, {io, format, ["2 triggered~n"]}, []),
    {error, ?ERR_HRS} = parse_entry(3, {"0-59/20", "25", '*', '*', '*'}, {io, format, ["3 triggered~n"]}, []),
    {error, ?ERR_DOM} = parse_entry(4, {"0-59/20", 12, 32, '*', '*'}, {io, format, ["4 triggered~n"]}, []),
    {error, ?ERR_MON} = parse_entry(5, {"0-59/20", 12, '*', "12,13", '*'}, {io, format, ["5 triggered~n"]}, []),
    {error, ?ERR_DOW} = parse_entry(6, {"0-59/20", 12, '*', 12, 8}, {io, format, ["6 triggered~n"]}, []),
    ok.
-else.
-endif.
