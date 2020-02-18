-module(saver).
-export([save/1]).

save(ListsFilenameForms) -> 
	lists:map(
		fun save_forms/1,
		ListsFilenameForms).

save_forms({Filename, Forms}) ->
	file:write_file(
		"./" ++ Filename, 
		list_to_binary(
			lists:flatten(
				[lists:flatten(erl_pp:form(Form))++"\n" || Form<-Forms]))).