-module(ast).
-export([getASTs/1]).

getASTs(FileOrDir) ->
	case filename:extension(FileOrDir) of 
		[] -> % It is a directory
			getErlASTs(FileOrDir);
		_ ->  % It is a file
			getFileASTs(FileOrDir)
	end.

getErlASTs(FileOrDir) ->
	case filename:extension(FileOrDir) of 
		[] -> % It is a directory
			{ok, Files} = file:list_dir(FileOrDir),
			lists:concat([getErlASTs(filename:absname(F, FileOrDir)) || F <- Files]);
		".erl" -> % An erlang file
			getFileASTs(FileOrDir);
		_ -> % It is not an erlang file
			[]
	end.

getFileASTs(File) ->
	case epp:parse_file(File, [], []) of 
		{ok, Forms} -> % The ast has been obtained
			[Forms];
		_ -> % An error has occurred
			[]
	end.