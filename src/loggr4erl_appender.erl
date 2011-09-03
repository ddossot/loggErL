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
  error_logger:info_msg("changed level to ~p~n",[Level]),
  {ok, State2};

handle_event({log, LLog}, State) ->
  do_log(LLog, State),
  {ok, State}.

handle_call({change_format, _Format}, State) ->
  error_logger:info_msg("ignoring format change: ~p~n", [State]),
  {ok, ok, State};

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(Info, State) ->
  error_logger:warning_msg("received unknown message: ~p~n", [Info]),
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
      post_loggr_event(ApiKey, LogKey, get_text(Log), get_fields(Log));
    false ->
      ok
  end.

post_loggr_event(ApiKey, LogKey, Text, OptionalFields) ->
  FormattedText = format_loggr_field(Text, text),
  FormattedOptionalFields = [{Name, format_loggr_field(Value, Name)} || {Name, Value} <- OptionalFields],
  
  case catch loggr_event:post(ApiKey, LogKey, FormattedText, FormattedOptionalFields) of
    ok ->
      ok;
    Error = {error, Cause} ->
      error_logger:error_msg("failed to post event (~1024p,~1024p) to loggr: ~1024p~n",
                             [FormattedText, FormattedOptionalFields, Cause]),
      Error;
    OtherError ->
      error_logger:error_msg("failed to post event (~1024p,~1024p) to loggr: ~1024p~n",
                             [FormattedText, FormattedOptionalFields, OtherError])
  end.

get_text(#log{msg=Msg, data=Data}) when is_list(Data) ->
  DataValues = [Value || {_, Value} <- Data],
  lists:flatten(io_lib:format(Msg, DataValues));
get_text(#log{msg=Msg, data=undefined}) ->
  Msg.
  
get_fields(Log=#log{data=undefined}) ->
  get_fields(Log#log{data=[]});
get_fields(#log{level=Level, data=Data}) ->
  [{tags, Level}|Data].
  
format_loggr_field(Term, FieldName) when is_atom(FieldName) ->
  {_, _, MaxSize} = lists:keyfind(FieldName, 1, ?LOGGR_EVENT_FIELDS),
  to_string(Term, MaxSize).

to_string(Term, MaxSize) when is_list(Term), is_integer(MaxSize) ->
  case catch lists:flatten(io_lib:format("~s", [Term])) of
    String when is_list(String) ->
      string:sub_string(String, 1, MaxSize);
    _ ->
      to_string(io_lib:format("~1024p",[Term]), MaxSize)
  end;
to_string(Term, MaxSize) when is_atom(Term), is_integer(MaxSize) ->
  to_string(atom_to_list(Term), MaxSize);
to_string(Term, MaxSize) when is_binary(Term), is_integer(MaxSize) ->
  to_string(binary_to_list(Term), MaxSize);
to_string(Term, MaxSize) when is_integer(MaxSize) ->
  to_string(io_lib:format("~1024p",[Term]), MaxSize).