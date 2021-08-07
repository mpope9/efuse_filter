%%-----------------------------------------------------------------------------
%% @copyright (C) 2021, Matthew Pope
%% @author Matthew Pope
%% @doc Interface for the fuse8 filter.
%%
%% Example usage:
%% ```
%% Filter = fuse8:new(["cat", "dog", "mouse"]),
%% true   = fuse8:contain(Filter, "cat"),
%% false  = fuse8:contain(Filter, "goose").
%% '''
%%
%% Incremental Init:
%% ```
%% Filter0 = fuse8:new_empty(),
%% Filter1 = fuse8:add([1, 2]),
%% Filter2 = fuse8:add([3, 4]),
%% Filter3 = fuse8:finalize(Filter2),
%% 
%% true = fuse8:contain(Filter3, 1),
%% false = fuse8:contain(Filter3, 5).
%%
%% Default Return Values:
%% `fuse8:contain/3' can return a default value.
%% Filter = fuse8:new(["Ricky Bobby", "Cal Naughton Jr."]),
%% true = fuse8:contain(Filter, "Ricky Bobby", {error, not_found}),
%% {error, not_found} = fuse8:contain(Filter, "Reese Bobby", {error, not_found}).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(efuse_filter).

-export([
    fuse8_initialize_nif_dirty/1,
    fuse8_initialize_nif/1,
    fuse8_contain_nif/2
]).

-on_load(init/0).

-define(APPNAME, efuse_filter).
-define(LIBNAME, efuse_filter).

fuse8_initialize_nif_dirty(_) ->
   not_loaded(?LINE).

fuse8_initialize_nif(_) ->
   not_loaded(?LINE).

fuse8_contain_nif(_, _) ->
   not_loaded(?LINE).

init() ->
   SoName = case code:priv_dir(?APPNAME) of
      {error, bad_name} ->
         case filelib:is_dir(filename:join(["..", priv])) of
            true ->
               filename:join(["..", priv, ?LIBNAME]);
            _ ->
               filename:join([priv, ?LIBNAME])
         end;
      Dir ->
         filename:join(Dir, ?LIBNAME)
   end,
   erlang:load_nif(SoName, 0).

not_loaded(Line) ->
   erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
