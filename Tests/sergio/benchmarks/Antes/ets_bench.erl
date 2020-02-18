-module(ets_bench).
-export([main/3]).

main(Insert1,Insert2,SearchId) ->
	TabId = createCollection(),
	deleteItem(TabId,{games,zelda}),
	addItem(TabId,{games,zelda}),
	addItem(TabId,{games,godOfWar}),
	addItem(TabId,{games,brainTraining}),
	addItem(TabId,{trilogies,theLordOfTheRings}),
	addItem(TabId,{trilogies,matrix}),
	addItem(TabId,{trilogies,blade}),
	addItem(TabId,{phones,nokia3310}),
	addItem(TabId,{phones,alcatelOT363}),
	addItem(TabId,{phones,lgL5}),
	Games = searchItem(TabId,games),
	addItem(TabId,Insert1),
	addItem(TabId,Insert2),
	Items = searchItem(TabId,SearchId),
	{Games,Items}.

createCollection() ->
	Id = ets:new(collection,[duplicate_bag]),
	addItem(Id,{games,pianoTiles2}),
	addItem(TabId,{games,zelda}),
	Id.

addItem(Tab,Input) ->
	{K,V} = Input,
	ets:insert(Tab,{K,V}).

deleteItem(Tab,Key) ->
	ets:delete_object(Tab,Key).

searchItem(Tab,Key) ->
	ets:lookup(Tab,Key).