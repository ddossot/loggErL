%%%
%%% @doc Client for all loggr operations.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2011 David Dossot
%%%
%%% @type loggr_event() = term(). Reference to the loggr_event parameterized module.

-module(loggerl).
-author('David Dossot <david@dossot.net>').
-include("loggerl.hrl").

-export([new_event/1]).

% TODO support binary fields?

%% @doc Create a new loggr event.
%% @spec new_event(Text::list()) -> LoggrEvent
%%   LoggrEvent = loggr_event()
new_event(Text) when is_list(Text) ->
 loggr_event:new(#loggr_event_fields{text=Text}).

