-module(eb_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case eb_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
    end.

stop(_State) ->
    exit(whereis(eb_sup), shutdown).
%% Then define a appname.app file to set application info.
%% 1> application:load(erlybank). -> load application.
%% 2> application:loaded_applications(). -> check applications already loaded
%% 3> application:start(erlybank). -> start applicaiton

