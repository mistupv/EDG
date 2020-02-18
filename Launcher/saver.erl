-module(saver).
-export([save/1]).

save(Forms) ->
	file:write_file("./tempSliced.erl", list_to_binary(lists:flatten([lists:flatten(erl_pp:form(Form))++"\n"||Form<-Forms]))).