%%%
%%% @doc log4erl appender that broadcasts to loggr.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2011 David Dossot
%%%

-module(loggr4erl_appender).

-behaviour(gen_event).

-include("loggerl.hrl").
-include_lib("log4erl/include/log4erl.hrl").

-record(configuration, {level=?DEFAULT_LEVEL, api_key, log_key}).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% gen_event functions
init({conf, Conf}) when is_list(Conf) ->
  {ok, #configuration{level=proplists:get_value(level, Conf),
                      api_key=proplists:get_value(api_key, Conf),
                      log_key=proplists:get_value(log_key, Conf)}}.

handle_event({change_level, Level}, State) ->
  State2 = State#configuration{level = Level},
  ?LOG2("changed level to ~p~n",[Level]),
  {ok, State2};

handle_event({log, LLog}, State) ->
  ?LOG2("handle_event:log = ~p~n",[LLog]),
  do_log(LLog, State),
  {ok, State}.

handle_call({change_format, _Format}, State) ->
  ?LOG2("ignoring format change: ~p~n",[State]),
  {ok, ok, State};

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  ?LOG2("~w received unknown message: ~p~n", [?MODULE, _Info]),
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
do_log(Log, #configuration{level=AppenderLevel, api_key=ApiKey, log_key=LogKey}) ->
  ShouldLog = log4erl_utils:to_log(Log#log.level, AppenderLevel),
  case ShouldLog of
    true ->
      loggr_event:post(ApiKey,
                       LogKey,
                       format_loggr_field(Log#log.msg, text),
                       [{tags, format_loggr_field(Log#log.level, tags)}]);
    false ->
      ok
  end.

format_loggr_field(Term, FieldName) when is_atom(FieldName) ->
  {_, _, MaxSize} = lists:keyfind(FieldName, 1, ?LOGGR_EVENT_FIELDS),
  to_string(Term, MaxSize).
  
to_string(Term, MaxSize) when is_list(Term), is_integer(MaxSize) ->
  string:sub_string(Term, 1, MaxSize);
to_string(Term, MaxSize) when is_atom(Term), is_integer(MaxSize) ->
  to_string(atom_to_list(Term), MaxSize);
to_string(Term, MaxSize) when is_binary(Term), is_integer(MaxSize) ->
  to_string(binary_to_list(Term), MaxSize);
to_string(Term, MaxSize) when is_integer(MaxSize) ->
  to_string(io:format("~1024p",[Term]), MaxSize).