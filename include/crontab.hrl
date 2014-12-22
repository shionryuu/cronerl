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
-author("ShionRyuu").

-record(entry, {
    name,           %% entry name
    min,            %% minute
    hour,           %% hour
    dom,            %%
    month,          %% month
    dow,            %%
    flags = 0,
    mfa,            %%
    options = []
}).

%% wildcard flags
-define(DOM_STAR, 1).
-define(DOW_STAR, 2).
-define(MIN_STAR, 8).
-define(HRS_STAR, 10).

%%
-define(FIRST_MIN, 0).
-define(LAST_MIN, 59).
-define(MIN_COUNT, (?LAST_MIN - ?FIRST_MIN + 1)).

-define(FIRST_HRS, 0).
-define(LAST_HRS, 23).
-define(HOUR_COUNT, (?LAST_HRS - ?FIRST_HRS + 1)).

-define(FIRST_DOM, 1).
-define(LAST_DOM, 31).
-define(DOM_COUNT, (?LAST_DOM - ?FIRST_DOM + 1)).

-define(FIRST_MON, 1).
-define(LAST_MON, 12).
-define(MON_COUNT, (?LAST_MON - ?FIRST_MON + 1)).

%% note on DOW: 0 and 7 are both Sunday, for compatibility reasons.
-define(FIRST_DOW, 0).  %% 0 for Sunday
-define(LAST_DOW, 7).
-define(DOW_COUNT, (?LAST_DOW - ?FIRST_DOW + 1)).

%% Month map
-define(MonMap, [
    {"Jan", 1}, {"Feb", 2}, {"Mar", 3}, {"Apr", 4}, {"May", 5}, {"Jun", 6},
    {"Jul", 7}, {"Aug", 8}, {"Sep", 9}, {"Oct", 10}, {"Nov", 11}, {"Dec", 12}
]).

%% Dow map
-define(DowMap, [
    {"Sun", 0}, {"Mon", 1}, {"Tue", 2}, {"Wed", 3},
    {"Thu", 4}, {"Fri", 5}, {"Sat", 6}, {"Sun", 7}
]).

%% if
-define(IF(Flag, T, F), case Flag of true -> T; _ -> F end).

%% error
-define(ERR_MIN, bad_min).
-define(ERR_HRS, bad_hrs).
-define(ERR_DOM, bad_dom).
-define(ERR_MON, bad_mon).
-define(ERR_DOW, bad_dow).

