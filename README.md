# TravianMap

A simple library to deal with the bad Travian "API". It is a best effort tool so when it faces against
an inconsistency of the Travian API, TravianMap discards it and continue. TranvianMap get the urls from the
[Travian Status](https://status.travian.com/) web page and get the map information appending /map.sql to the
server urls.

It uses [inets](https://www.erlang.org/doc/man/inets.html) for fetching the data and [thoas](https://github.com/lpil/thoas) to handle the json format.

I develop TravianMap for using it in my personal [Travian project](https://github.com/SirWerto/Mi-intento-final-de-Travian)
but I decided to split it from the main project because some Travianers could need it in the future :).


## Installation

### Erlang

```erlang
% rebar.config
{deps, [travianmap]}
```


### Elixir

```elixir
# mix.exs
def deps do
  [{:travianmap, "~> 0.1"}]
end
```



## Basic Usage

```erlang
1> travianmap:get_urls().
{ok,[<<"https://ts5.x1.america.travian.com">>,
     <<"https://ts4.x1.america.travian.com">>,
     <<"https://ts15.x1.america.travian.com">>,
     <<"https://ts8.x1.america.travian.com">>,
     <<"https://ts25.x2.america.travian.com">>,
     <<"https://ts7.x1.america.travian.com">>,
     <<"https://ts2.x1.america.travian.com">>,
     <<"https://ts50.x5.america.travian.com">>,
     <<"https://ts6.x1.america.travian.com">>,
     <<"https://ts15.x1.arabics.travian.com">>]}

2> Url = <<"https://ts50.x5.america.travian.com">>.

3> travianmap:get_info(Url).
{ok,#{<<"18c">> => false,<<"adventuresDecay">> => false,
      <<"allianceBanner">> => true,<<"allianceBonus">> => true,
      <<"allianceShowIncomingAttacks">> => false,
      <<"boostedStart">> => false,<<"bottom">> => -200,
      <<"cities">> => false,<<"contextHelp">> => true,
      <<"country">> => <<"com">>,<<"factions">> => false,
      <<"height">> => 401,<<"hideFoolsArtifacts">> => false,
      <<"left">> => -200,<<"lockingRegionsAgain">> => true,
      <<"multi_language">> => true,<<"progressiveTasks">> => true,
      <<"rearrangeBuildings">> => true,
      <<"resourcesInHeroBag">> => true,<<"right">> => 200,
      <<"sittingOnlyFriends">> => true,<<"speed">> => 5,
      <<"territory">> => false,<<"title">> => <<"America 50">>,
      <<"top">> => 200,<<"travelOverTheWorldEdge">> => true,
      <<"tribesEgyptiansAndHuns">> => false,...}}

4> {ok, ServerMap} = travianmap:get_map(Url)
5> {ok, Villages} = travianmap:parse_map(ServerMap, no_filter).
[{ok,{318,117,200,1,24785,<<"Tánatos"/utf8>>,2916, <<"Gluk">>,0,<<>>,580}},
 {ok,{388,187,200,1,24830,<<"Fornost">>,328,<<"Boromir">>,34, <<"L1389">>,542}},
 {ok,{389,188,200,1,24416,<<"Eldalondë"/utf8>>,328, <<"Boromir">>,34,<<"L1389">>,711}},
 {ok,{390,189,200,1,26671,<<"Aldea nueva">>,328, <<"Boromir">>,34,<<"L1389">>,98}},
 {error, unable_to_parse},
 {ok,{391,190,200,1,21867,<<"Edhellond">>,328,<<"Boromir">>, 34,<<"L1389">>,844}},
 {ok,{392,191,200,1,19940,<<"Abismo de Helm">>,328, <<"Boromir">>,34,<<"L1389">>,1009}},
 {ok,{717,115,199,1,25112,<<"Hipnos">>,2916,<<"Gluk">>,0, <<>>,472}}| ...]

6> {ok, Villages} = travianmap:parse_map(ServerMap, filter).
[{318,117,200,1,24785,<<"Tánatos"/utf8>>,2916, <<"Gluk">>,0,<<>>,580},
 {388,187,200,1,24830,<<"Fornost">>,328,<<"Boromir">>,34, <<"L1389">>,542},
 {389,188,200,1,24416,<<"Eldalondë"/utf8>>,328, <<"Boromir">>,34,<<"L1389">>,711},
 {390,189,200,1,26671,<<"Aldea nueva">>,328, <<"Boromir">>,34,<<"L1389">>,98},
 {391,190,200,1,21867,<<"Edhellond">>,328,<<"Boromir">>, 34,<<"L1389">>,844},
 {392,191,200,1,19940,<<"Abismo de Helm">>,328, <<"Boromir">>,34,<<"L1389">>,1009},
 {717,115,199,1,25112,<<"Hipnos">>,2916,<<"Gluk">>,0, <<>>,472}| ...]
```