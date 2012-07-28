-module(chat).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/2, stop/1]).
-export([init/2]).

start(User, Password) ->
  spawn(?MODULE, init, [User, Password]).

stop(EchoClientPid) ->
  EchoClientPid ! stop.

init(User, Password) ->
  application:start(exmpp),
  Session = exmpp_session:start({1,0}),
  JID = exmpp_jid:parse(User),
  exmpp_session:auth_info(Session, JID, Password),
  {ok, _StreamId, _Features} = exmpp_session:connect_TCP(Session, "talk.l.google.com", 5222, [{starttls, enabled}]),
  session(Session).

session(Session) ->
  exmpp_session:login(Session, "PLAIN"),
  exmpp_session:send_packet(Session,
    exmpp_presence:set_status(
      exmpp_presence:available(), "Echo Ready")),
  loop(Session, []).

loop(Session, UsersOnline) ->
  io:format("Users:~n~p~n~n", [UsersOnline]),
  receive
    stop ->
      exmpp_session:stop(Session);
    Record = #received_packet{packet_type=message, raw_packet=Packet, type_attr=Type} when Type =/= "error" ->
      %io:format("Received Message stanza:~n~p~n~n", [Record]),
      send_message(Session, Packet, UsersOnline),
      loop(Session, UsersOnline);
    Record when Record#received_packet.packet_type == 'presence' ->
      io:format("Received Presence stanza:~n~p~n~n", [Record]),
      loop(Session, [Record#received_packet.from | UsersOnline]);
    Record ->
      loop(Session, UsersOnline)
  end.


send_message(Session, Packet, [To | Others]) -> 
  echo_packet(Session, Packet, To),
  send_message(Session, Packet, Others);

send_message(_Session, _Packet, []) -> ok.

echo_packet(Session, Packet, To) ->
  Jid = exmpp_jid:make(To),
  JidString = exmpp_jid:to_list(Jid),
  TmpPacket = exmpp_xml:remove_attribute(Packet, <<"from">>),
  TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, JidString), 
  NewPacket = exmpp_xml:remove_attribute(TmpPacket2, <<"id">>),
  io:format("Message: ~n~p~n", [NewPacket]),
  exmpp_session:send_packet(Session, NewPacket).

handle_presence(Session, Packet, _Presence) ->
  case exmpp_jid:make(_From = Packet#received_packet.from) of
    JID ->
      case _Type = Packet#received_packet.type_attr of
        "available" ->
          ok;
        "unavailable" ->
          ok;
        "subscribe" ->
          presence_subscribed(Session, JID),
          presence_subscribe(Session, JID);
        "subscribed" ->
          presence_subscribed(Session, JID),
          presence_subscribe(Session, JID)
      end
  end.

presence_subscribed(Session, Recipient) ->
  Presence_Subscribed = exmpp_presence:subscribed(),
  Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
  exmpp_session:send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
  Presence_Subscribe = exmpp_presence:subscribe(),
  Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
  exmpp_session:send_packet(Session, Presence).
