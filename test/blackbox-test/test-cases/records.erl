-module(records).

-export([create/0, field/1, field_index/0, update/1, record_pattern/1]).

-record(rec, {foo :: integer(), bar :: string()}).

-spec create() -> #rec{}.
create() ->
    #rec{foo = 0, bar = "BAR"}.

-spec field(#rec{}) -> number().
field(Rec) ->
    Rec#rec.foo.

-spec field_index() -> integer().
field_index() ->
    #rec.foo.

-spec update(#rec{}) -> #rec{}.
update(Rec) ->
    Rec#rec{bar = "BAR2"}.

-spec record_pattern(#rec{}) -> {ok, integer()} | {error, string()}.
record_pattern(Rec) ->
    case Rec of
        #rec{foo = Foo, bar = "YES"} -> {ok, Foo};
        #rec{foo = Foo, bar = Bar} -> {error, Bar}
    end.