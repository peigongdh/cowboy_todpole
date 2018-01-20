-module(ws_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  Msg = #{<<"type">> => <<"welcome">>, <<"id">> => State},
  MsgEncode = jsone:encode(Msg),
  {reply, {text, MsgEncode}, State}.

websocket_handle({text, Msg}, State) ->
  MsgDecode = jsone:decode(Msg),
  #{<<"type">> := Type} = MsgDecode,
  if
    Type == <<"login">> ->
      {ok, State};
    Type == <<"update">> ->
      UpdateMsg = MsgDecode#{
        <<"id">> => State
      },
      MsgEncode = jsone:encode(UpdateMsg),
      {reply, {text, MsgEncode}, State};
    Type == <<"message">> ->
      UpdateMsg = MsgDecode#{
        <<"id">> => State
      },
      MsgEncode = jsone:encode(UpdateMsg),
      {reply, {text, MsgEncode}, State};
    Type == <<"shoot">> ->
      UpdateMsg = MsgDecode#{
        <<"id">> => State
      },
      MsgEncode = jsone:encode(UpdateMsg),
      {reply, {text, MsgEncode}, State};
    true ->
      {ok, State}
  end;
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.