%%%
%%% @doc loggr Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2011 David Dossot
%%%

%                            {name, min, max}
-define(LOGGR_EVENT_FIELDS, [{text,   1, 500},
                             {link,   0, 200},
                             {source, 0, 200},
                             {user,   0, 200},
                             {tags,   0, 200},
                             {value,  0, 30},
                             {data,   0, 5120},
                             {geo,    0, 30}]).
