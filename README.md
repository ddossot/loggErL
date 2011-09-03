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


## Building

**loggErL** relies on [rebar](http://bitbucket.org/basho/rebar/wiki/Home) for its build and dependency management and targets Erlang/OTP R13B04 or above.

Fetch the dependencies and compile with:

    rebar get-deps compile
    
To run the test suite:

    rebar skip_deps=true eunit

To generate the **loggErL** documentation:

    rebar skip_deps=true doc


## Usage

Pre-requisite:

- The ibrowse application must be started prior to using **loggErL**.

### Direct API calls

Posting a simple event:

    loggr_event:post(ApiKey, LogKey, "a new event").

Posting an event with [optional fields](http://docs.loggr.net/events):

    loggr_event:post(ApiKey, LogKey, "an event with data", [{data, "some\ndata"}]).
    
It's also possible to configure globally the ApiKey and LogKey in the application configuration:

    {env,[{loggr_api_key, "..."}, {loggr_log_key,"..."}]}

With this in place, you can do:

    loggr_event:post("a new event").
    loggr_event:post("an event with data", [{data, "some\ndata"}]).

### Log4Erl Appender

This appender allows broadcasting [Log4Erl](https://github.com/ahmednawras/log4erl) events to loggr.

Its configuration is very basic:

    loggr4erl_appender loggr {
      level = info,
      api_key = "...",
      log_key = "..."
    }

With this in place, you can broadcast events to loggr from Log4Erl:

    log4erl:warn("something is not right").
  
> The Log4Erl log level (debug, info...) is propagated as a loggr tag.

Any loggr [optional field](http://docs.loggr.net/events) can be passed too, as shown in the following examples:

    log4erl:info("user ~p just logged in~i", [{user, "u123"}, {geo, "40.1203,-76.2944"}]).
    
    log4erl:warn("something went really bad: ~p", [{data, erlang:get_stacktrace()}]).


## Pending Features

- Client for the Loggr Web Api.


#### Copyright 2011 - David Dossot - MIT License
