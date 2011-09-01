<pre>
  .                        .____        .    
  |     __.    ___.   ___. /      .___  /    
  |   .'   \ .'   ` .'   ` |__.   /   \ |    
  |   |    | |    | |    | |      |   ' |    
 /\__  `._.'  `---|  `---| /----/ /     /---/
              \___/  \___/                   
</pre>

# loggr Erlang Client

A client for [loggr](http://loggr.net).

### Usage

Pre-requisite:

- The ibrowse application must be started prior to using *loggErL*.


Posting a simple event:

    loggr_event:post(ApiKey, LogKey, "a new event").

Posting an event with [optional fields](http://docs.loggr.net/events):

    loggr_event:post(ApiKey, LogKey, "an event with data", [{data, "some\ndata"}]).


### Pending

- Support setting ApiKey and LogKey in *loggErL*'s application environment,
- Log4Erl integration,
- Client for the Loggr Web Api.


### Building

*loggErL* relies on [rebar](http://bitbucket.org/basho/rebar/wiki/Home) for its build and dependency management and targets Erlang/OTP R13B04 or above.

Simply run:

    rebar get-deps compile
    
To run the test suite:

    rebar skip_deps=true eunit

To generate the *loggErL* documentation, run:

    rebar skip_deps=true doc



#### Copyright 2011 - David Dossot - MIT License
