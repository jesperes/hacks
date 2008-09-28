%% A builder takes a Node as input and brings it up to date.

-module(builder).
-record(builder, { env, 
		   actions }).

create_builder(Env, Actions) ->
    #builder{env = Env, actions = Actions}.

actions(Builder) ->
    Builder#builder.actions.

build(Builder, Node) ->
    io:format("Building node: ~p~n", [Node]),
    io:format("Actions: ~p~n", [actions(Builder)]).
   
