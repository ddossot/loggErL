%%%
%%% @doc loggr event.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2011 David Dossot
%%%

-module(loggr_event).
-author('David Dossot <david@dossot.net>').
-include("loggerl.hrl").
-import(error_m,[return/1,fail/1]).
-compile({parse_transform, do}).

-export([post/1, post/2, post/3, post/4]).

% TODO support binary fields?
% TODO allow setting current stack as text

%% @doc Post a text-only event to the loggr server, using the loggr_api_key and loggr_log_key application configuration parameter.
%% @spec post(Text::string()) -> ok | {error, Error::term()}
post(Text) when is_list(Text) ->
  
  post(Text, []).

%% @doc Post an event to the loggr server, using the loggr_api_key and loggr_log_key application configuration parameter.
%% @spec post(Text::string(), OptionalFields::proplist()) -> ok | {error, Error::term()}
post(Text, OptionalFields) when is_list(Text), is_list(OptionalFields) ->
  
  do([error_m ||
      ApiKey <- get_application_configuration_parameter(loggr_api_key),
      LogKey <- get_application_configuration_parameter(loggr_log_key),
      post(ApiKey, LogKey, Text, OptionalFields)]).

%% @doc Post a text-only event to the loggr server.
%% @spec post(ApiKey::string(), LogKey::string(), Text::string()) -> ok | {error, Error::term()}
post(ApiKey, LogKey, Text)
  when is_list(ApiKey), is_list(LogKey), is_list(Text) ->
  
  post(ApiKey, LogKey, Text, []).

%% @doc Post an event to the loggr server.
%% @spec post(ApiKey::string(), LogKey::string(), Text::string(), OptionalFields::proplist()) -> ok | {error, Error::term()}
post(ApiKey, LogKey, Text, OptionalFields)
  when is_list(ApiKey), is_list(LogKey), is_list(Text), is_list(OptionalFields) ->
  
  Fields = [{text, Text}|OptionalFields],
  
  do([error_m ||
      ensure_fields_format(Fields),
      validate_fields(Fields),
      perform_post(ApiKey, LogKey, Fields)]).
      
%% Private functions
get_application_configuration_parameter(Name) when is_atom(Name) ->
  case application:get_env(loggerl, Name) of
    {ok, Value} -> return(Value);
    _ -> fail({missing_application_configuration_parameter, Name})
  end.

ensure_fields_format([]) ->
  return(ok);
ensure_fields_format([Field|Rest]) ->
  case Field of
    {Name, Value} when is_atom(Name), is_list(Value) ->
      ensure_fields_format(Rest);
    _ ->
      fail({bad_field_format, Field})
  end.
  
validate_fields(Fields) ->
  validate_fields(Fields, ?LOGGR_EVENT_FIELDS).

validate_fields([], []) ->
  return(ok);
validate_fields(UnknownFields, []) ->
  fail({unknown_fields, UnknownFields});
validate_fields(Fields, [{Name,Min,Max}|Rest]) ->
  Value = proplists:get_value(Name, Fields, ""),  
  
  case ValueLength = length(Value) of
    _ when ValueLength < Min ->
      fail({required_field, Name});
    _ when ValueLength > Max ->
      fail({value_to_big_for_field, Name, Max});
    _ ->
      validate_fields(proplists:delete(Name, Fields), Rest)
  end.
  
perform_post(ApiKey, LogKey, Fields) ->
  EventsUrl = "http://post.loggr.net/1/logs/" ++ LogKey ++ "/events",
  
  EventBody =
    lists:foldl(
      fun({Name, Value}, Acc) ->
        Acc ++ "&" ++ atom_to_list(Name) ++ "=" ++ ibrowse_lib:url_encode(Value)
      end,
      "apikey=" ++ ApiKey,
      Fields),
      
  Res = ibrowse:send_req(EventsUrl, [{"Content-Type", "application/x-www-form-urlencoded"}], post, EventBody),
  
  case Res of
    {ok,"201", _, _} ->
      ok;
    Other ->
      {error, Other}
  end.

%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ensure_fields_format_test() ->
  ?assertEqual(ok, ensure_fields_format([{foo, "hello"}])),
  ?assertEqual({error,{bad_field_format,{"foo","bar"}}}, ensure_fields_format([{"foo", "bar"}])),
  ?assertEqual({error,{bad_field_format,{foo, bar}}}, ensure_fields_format([{foo, bar}])),
  ok.

validate_fields_test() ->
  ?assertEqual(ok, validate_fields([{text, "hello"}])),
  ?assertEqual({error,{required_field,text}}, validate_fields([{geo, "40.1203,-76.2944"}])),
  ?assertEqual({error,{value_to_big_for_field,value,30}}, validate_fields([{text, "hello"},{value, "1234567890123456789012345678901"}])),
  ?assertEqual({error,{unknown_fields,[{unsupported,"foo"}]}}, validate_fields([{text, "hello"},{unsupported, "foo"}])),
  ok.
  
-endif.
