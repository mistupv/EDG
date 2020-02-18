-module(ast).
-export([getAST/1]).

getAST(File) ->
	% c(fichero). % para compilar desde el interprete de erlang sin "" ni .erl
	% compile:file(File), % para compilar desde dentro de un fichero
	{ok, Forms} = epp:parse_file(File, [], []),
	Forms.