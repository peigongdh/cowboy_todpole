-module(ws_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  erlang:start_timer(1000, self(), <<"Hello!">>),
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  MsgDecode = jsone:decode(Msg),
  MsgEncode = jsone:encode(MsgDecode),
  {reply, {text, <<"That's what she said! ", MsgEncode/binary>>}, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
  erlang:start_timer(1000, self(), <<"How' you doin'?">>),
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.