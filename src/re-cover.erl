-module ('re-cover').

-export (
   [
	start/0, stop/0, cover/2, analyse/1, analyse/2, dump_analysis/1, modules/0
   ]
  ).

%% API

start() ->
	case cover:start() of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid};
		_ -> error
	end.

stop() -> cover:stop().

cover([], _Type) -> ok;
cover([Dir | Dirs], Type) when is_atom(Type) ->
	{ok, _Pid} = start(),
	traverse(Dir, Type),
	cover(Dirs,Type).

dump_analysis(DirOut0) ->
	Modules = cover:modules(),
	DirOut1 = DirOut0 ++ "/wg_dbg_cover-" ++ time_string() ++ "/",
	F = fun (Module) -> analyse(Module, DirOut1) end,
	ok = lists:foreach(F, Modules).

analyse(Module) ->
	case cover:is_compiled(Module) of
		{file, _} -> cover:analyse(Module);
		Else -> Else
	end.

analyse(Module, OutputDir) ->
	case cover:is_compiled(Module) of
		{file, File} ->
			io:format("=> ~p.\n", [File]),
			OutputFile = OutputDir ++ "/" ++ File,
			ok = filelib:ensure_dir(OutputFile),
			case cover:analyse_to_file(Module, OutputFile) of
				{ok, _} -> {ok, Module};
				{error, no_source_code_found} -> analyse_term(Module, OutputFile);
				{error, Error} -> {error, Module, Error}
			end;
		false -> {error, Module, module_not_compiled};
		{error, Error} -> {error, Module, Error}
	end.

modules() -> cover:modules().

%% Internal

analyse_term(Module, File) ->
	case cover:analyse(Module) of
		{ok, Result} ->
			case file:write_file(File, io_lib:fwrite("~p", [Result])) of
				ok ->
					{ok, Module};
				{error, Reason} ->
					{error, Module, Reason}
			end;
		{error, Reason} ->
			{error, Module, Reason}
	end.

time_string() ->
	{H, M, S} = time(),
	io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).

traverse(Dir, src) when is_list(Dir) -> traverse(Dir, [], "src");
traverse(Dir, beam) when is_list(Dir) -> traverse(Dir, [], "ebin");
traverse(_, _) -> [].

get_module_name(File) ->
    Start = string:rchr(File, $/),
    End = string:rchr(File, $.),
    string:substr(File, Start + 1, End - Start - 1).

compile_dir(For, PathSpec0) ->
    PathSpec1 = lists:reverse([For, "/" | PathSpec0]),
    Path = lists:concat(PathSpec1),

	io:format("Compiling: ~s\n", [Path]),

    Filenames =
		case For of
			"src" ->
				_ = cover:compile_directory(Path),
				filelib:wildcard(Path ++ "/*.erl");
			"ebin" ->
				_ = cover:compile_beam_directory(Path),
				filelib:wildcard(Path ++ "/*.beam")
		end,

    F = fun (File, Acc) ->
                case filelib:is_file(File) of
                    true -> [get_module_name(File)|Acc];
                    false -> Acc
                end
        end,
    _ErlMods = lists:foldl(F, [], Filenames).

traverse(Dir, PathAcc, For) ->
	io:format("Traversing ~s\n", [Dir]),
    {ok, Cwd} = file:get_cwd(),
    Modules =
        case file:set_cwd(Dir) of
            ok ->
                {ok, Filenames} = file:list_dir("."),
                F = fun(Name, Acc) ->
                            %% For the sake of simplicity, do it the dirty way
                            case {filelib:is_dir(Name), Name == For} of
                                {true, true} ->
                                    Acc ++ compile_dir(Name, [Dir | PathAcc]);
                                {true, false} ->
                                    Acc ++ traverse(Name, ["/", Dir | PathAcc], For);
                                {false, _} -> Acc
                            end
                    end,
                lists:foldl(F, [], Filenames);
            {error, _Reason} -> []
        end,
    file:set_cwd(Cwd),
    Modules.
