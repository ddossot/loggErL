%%%
%%% @doc loggr event.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2011 David Dossot
%%%

-module(loggr_event, [Event]).
-author('David Dossot <david@dossot.net>').
-include("loggerl.hrl").

-export([link/1, source/1,
         post/2]).

% FIXME enforce size limits http://docs.loggr.net/events

%% @doc Set the link field on the current event.
%% @spec link(Link::list()) -> LoggrEvent
%%   LoggrEvent = loggr_event()
link(Link) when is_list(Link) ->
  loggr_event:new(Event#loggr_event_fields{link=Link}).

%% @doc Set the source field on the current event.
%% @spec source(Source::list()) -> LoggrEvent
%%   LoggrEvent = loggr_event()
source(Source) when is_list(Source) ->
  loggr_event:new(Event#loggr_event_fields{source=Source}).

% FIXME support all fields

% TODO allow setting current stack as text

%% @doc Post the current event to the loggr server.
%% @spec post(ApiKey::list(), LogKey::list()) -> ok
post(ApiKey, LogKey) when is_list(ApiKey), is_list(LogKey) ->
  EventsUrl = "http://post.loggr.net/1/logs/" ++ LogKey ++ "/events",
  % FIXME support all fields, in a cleaner way :)
  EventBody = "apikey=" ++ ApiKey ++ "&text=" ++ ibrowse_lib:url_encode(Event#loggr_event_fields.text),
  Res = ibrowse:send_req(EventsUrl, [{"Content-Type", "application/x-www-form-urlencoded"}], post, EventBody),
  % FIXME handle result
  io:format("~1024p~n", [Res]),
  ok.

% FIXME support post/0 getting LogKey/ApiKey from app config

