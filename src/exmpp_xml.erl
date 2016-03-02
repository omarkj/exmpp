-module(exmpp_xml).
-include_lib("exml/include/exml_stream.hrl").
-include_lib("exml/include/exml.hrl").

-export([parser/0,
         parse/2,
         encode/1]).

-type els() :: [el()].
-type attrs() :: [attr()].
-type attr() :: #{name => iolist(),
                  val => iolist()}.
-type el() :: #{name => iolist(),
                attrs => attrs(),
                data => iolist(),
                childs => [el()]}|
              #{name => iolist(),
                type => stream_start,
                attrs => attrs()}.
-type parser() :: exml_stream:parser().
-export_type([parser/0,
              el/0]).

-spec parser() -> Parser when
      Parser :: parser().
parser() ->
    {ok, Parser} = exml_stream:new_parser(),
    Parser.

-spec parse(Payload, Parser) -> Result when
      Payload :: binary(),
      Parser :: parser(),
      Result :: Ok | Error,
      Ok :: {ok, els(), Parser},
      Error :: {error, term()}.
parse(Payload, Parser) ->
    case exml_stream:parse(Parser, Payload) of
        {ok, Parser1, Events} ->
            Events1 = decode(Events),
            {ok, lists:reverse(Events1), Parser1};
        {error, E} -> {error, E}
    end.

-spec encode(els()) -> iolist().
encode(Els) ->
    exml:to_binary(encode(Els, [])).

encode([], Retval) ->
    lists:reverse(Retval);
encode([#{type := stream_end, name := Name}|Els], Retval) ->
    EEl = #xmlstreamend{name = Name},
    encode(Els, [EEl|Retval]);
encode([#{type := stream_start, name := Name}=El|Els], Retval) ->
    Attrs = attrs(maps:get(attrs, El, [])),
    EEl = #xmlstreamstart{name = Name,attrs = Attrs},
    encode(Els, [EEl|Retval]);
encode([#{name := Name}=El|Els], Retval) ->
    Attrs = attrs(maps:get(attrs, El, #{})),
    Childs = encode(maps:get(childs, El, []), []),
    Childs1 = case maps:get(data, El, undefined) of
                  undefined -> Childs;
                  Data -> [#xmlcdata{content=Data}|Childs]
              end,
    EEl = #xmlel{
             name = Name,
             attrs = Attrs,
             children = Childs1
            },
    encode(Els, [EEl|Retval]).

attrs(Attrs) -> maps:to_list(Attrs).

decode(Events) ->
    decode(Events, []).

decode([], Retval) ->
    Retval;
decode([#xmlstreamstart{attrs = Attrs,
                        name = Name}|Events], Retval) ->
    decode(Events, [#{type => stream_start,
                      name => Name,
                      attrs => decode_attrs(Attrs)}|Retval]);
decode([#xmlstreamend{}|Events], Retval) ->
    decode(Events, [stream_end|Retval]);
decode([#xmlcdata{content=Content}|Events], Retval) ->
    decode(Events, [Content|Retval]);
decode([#xmlel{name=Name,attrs=Attrs,
               children=Children}|Events], Retval) ->
    Childs = decode(Children, []),
    Attrs1 = decode_attrs(Attrs),
    DEl1 = case decode(Children, []) of
               [Data] when is_binary(Data) ->
                   #{name => Name,
                     attrs => Attrs1,
                     data => Data,
                     childs => []};
               Childs ->
                   #{name => Name,
                     attrs => Attrs1,
                     childs => Childs}
           end,
    decode(Events, [DEl1|Retval]).

decode_attrs(Attrs) ->
    lists:foldl(
      fun({AName, AVal}, OutMap) ->
              OutMap#{AName => AVal}
      end, #{}, Attrs).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

encode_test_() ->
    [?_assertEqual(
        <<"<test/>">>,
        iolist_to_binary(encode([#{name => <<"test">>}]))),
     ?_assertEqual(
        <<"<test/><test/>">>,
        iolist_to_binary(encode([#{name => <<"test">>},
                                 #{name => <<"test">>}]))),
     ?_assertEqual(
        <<"<test foo='bar'/>">>,
        iolist_to_binary(encode([#{name => <<"test">>,
                                   attrs => #{<<"foo">> => <<"bar">>}}]))),
     ?_assertEqual(
        <<"<test bar='foo' foo='bar'/>">>,
        iolist_to_binary(encode([#{name => <<"test">>,
                                   attrs => #{<<"foo">> => <<"bar">>,
                                              <<"bar">> => <<"foo">>}}]))),
     ?_assertEqual(
        <<"<test>bar</test>">>,
        iolist_to_binary(encode([#{name => <<"test">>,
                                   data => <<"bar">>}]))),
     ?_assertEqual(
        <<"<test><test/></test>">>,
        iolist_to_binary(encode([#{name => <<"test">>,
                                   childs => [#{name => <<"test">>}]}])))].

-endif.
