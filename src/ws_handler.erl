-module(ws_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(_State) ->
  cowboy_todpole_server:register(self()),
  UidIndex = ets:lookup(user_info, uid_index),
  Uid =
    case UidIndex of
      [{uid_index, Index}] ->
        NUid = Index + 1,
        ets:insert(user_info, {uid_index, NUid}),
        ets:insert(user_info, {user, {uid, Index}}),
        Index;
      _ ->
        1
    end,
  Msg = #{<<"type">> => <<"welcome">>, <<"id">> => Uid},
  MsgEncode = jsone:encode(Msg),
  NewState = #{uid => Uid},
  {reply, {text, MsgEncode}, NewState}.

websocket_handle({text, Msg}, State) ->
  Uid = maps:get(uid, State),
  MsgDecode = jsone:decode(Msg),
  #{<<"type">> := Type} = MsgDecode,
  if
    Type == <<"login">> ->
      {ok, State};
    Type == <<"update">> ->
      UpdateMsg = MsgDecode#{
        <<"id">> => Uid,
        <<"name">> => <<"Guest.", (integer_to_binary(Uid))/binary>>
      },
      MsgEncode = jsone:encode(UpdateMsg),
      % {reply, {text, MsgEncode}, State};
      cowboy_todpole_server:msg(MsgEncode),
      {ok, State};
    Type == <<"message">> ->
      UpdateMsg = MsgDecode#{
        <<"id">> => Uid
      },
      MsgEncode = jsone:encode(UpdateMsg),
      % {reply, {text, MsgEncode}, State};
      cowboy_todpole_server:msg(MsgEncode),
      {ok, State};
    Type == <<"shoot">> ->
      UpdateMsg = MsgDecode#{
        <<"id">> => Uid
      },
      MsgEncode = jsone:encode(UpdateMsg),
      % {reply, {text, MsgEncode}, State};
      cowboy_todpole_server:msg(MsgEncode),
      {ok, State};
    true ->
      {ok, State}
  end;
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({msg, Msg}, State) ->
  {reply, {text, Msg}, State};
websocket_info({timeout, _Ref, Msg}, State) ->
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.