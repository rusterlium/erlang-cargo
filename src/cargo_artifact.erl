-module(cargo_artifact).

-export([
    from_json/1,

    name/1,
    version/1,
    version/2,
    package_id/1,
    fresh/1,
    kind/1,
    filenames/1,
    executable/1
]).


-record(artifact, {
    name :: binary(),
    % crate :: binary(), from package_id?
    version :: binary() | undefined,
    package_id :: binary(),
    fresh :: boolean(),
    kind :: kind(),
    filenames :: [binary()],
    executable :: binary() | undefined
}).

-type kind() :: bin | cdylib | other.
-opaque t() :: #artifact{}.
-export_type([t/0]).

-spec from_json(jsx:term()) -> {ok, t()} | error.
from_json(Entry) ->
    case maps:get(<<"reason">>, Entry, undefined) of
        <<"compiler-artifact">> ->
            #{
                <<"package_id">> := PackageId,
                <<"target">> := Target
             } = Entry,
            Kind = kind_to_atom(maps:get(<<"kind">>, Target)),
            Executable = case maps:get(<<"executable">>, Entry, undefined) of
                null -> undefined;
                V -> V
            end,
            {ok, #artifact{
                name = maps:get(<<"name">>, Target),
                filenames = maps:get(<<"filenames">>, Entry),
                kind = Kind,
                package_id = PackageId,
                fresh = maps:get(<<"fresh">>, Entry, false),
                executable = Executable
            }};
        _ ->
            error
    end.


-spec name(t()) -> binary().
name(#artifact{name=C}) -> C.

-spec version(t()) -> binary() | undefined.
version(#artifact{version=V}) -> V.

-spec version(t(), binary()) -> t().
version(#artifact{} = A, V) -> A#artifact{version=V}.

-spec package_id(t()) -> binary().
package_id(#artifact{package_id=P}) -> P.

-spec fresh(t()) -> boolean().
fresh(#artifact{fresh=F}) -> F.

-spec kind(t()) -> kind().
kind(#artifact{kind=K}) -> K.

-spec filenames(t()) -> [binary()].
filenames(#artifact{filenames=F}) -> F.

-spec executable(t()) -> binary() | undefined.
executable(#artifact{executable=E}) -> E.


kind_to_atom([<<"bin">>]) -> bin;
kind_to_atom([<<"cdylib">>]) -> cdylib;
kind_to_atom(_) -> other.