-module(decorator_test).
-include_lib("eunit/include/eunit.hrl").
-export([replace_return_value_decorator/2]).

-compile([{parse_transform, decorators}]).


% example decorator that replaces the return value with the atom 'replaced'
% note that we always pass the arguments as a single list to the next fun
replace_return_value_decorator(F,Args)->
	fun() -> 
		_R = apply(F, [Args] ),
		replaced
	end.

-decorate({ ?MODULE, replace_return_value_decorator }).
replace_ret_val_decorated() -> ok.

replace_ret_value_test()->
	?assertEqual(replaced, replace_ret_val_decorated() ).

