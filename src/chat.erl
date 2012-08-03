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
  JID = exmpp_jid:parse(User ++ "/bot"),
  exmpp_session:auth_info(Session, JID, Password),
  {ok, _StreamId, _Features} = exmpp_session:connect_TCP(Session, "talk.l.google.com", 5222, [{starttls, enabled}]),
  session(Session).

session(Session) ->
  exmpp_session:login(Session, "PLAIN"),
  send(Session, exmpp_presence:set_status(exmpp_presence:available(), "Sacudiendo La Cabeza")),
  Room = muc_join(Session),
  send(Session, get_rooster()),
  loop(Session, [], Room, nil).

get_my_jid(nil, Packet) ->
  exmpp_stanza:get_recipient(Packet);
get_my_jid(JID, _) ->
  JID.

loop(Session, UsersOnline, Room, Me) ->
  receive
    stop ->
      exmpp_session:stop(Session);
    Record = #received_packet{raw_packet=Packet} ->
      MyJID = get_my_jid(Me, Packet),
      {users, UserList} = handle_packet(Session, MyJID, Room, UsersOnline, Record),
      loop(Session, UserList, Room, MyJID);
    Message ->
      io:format("Unknown type of message:~n~p~n~n", [Message]),
      loop(Session, UsersOnline, Room, Me)
  end.

get_rooster() ->
  Query = exmpp_xml:set_attributes(
      #xmlel{ns = ?NS_ROSTER, name = 'query'},
      [{<<"xmlns:gr">>, "google:roster"}, {<<"gr:ext">>, "2"}]),
  Iq = exmpp_xml:set_attributes(
      #xmlel{ns = ?NS_JABBER_CLIENT, name = 'iq'},
      [{<<"type">>, "get"}, {<<"id">>, "grooster_1"}]),
    exmpp_xml:append_child(Iq, Query).


muc_join(Session) ->
  UUID = uuid:to_string(uuid:v4()),
  JID = "private-chat-" ++ UUID ++ "@groupchat.google.com/sacudiendo", 
	Packet = exmpp_xml:append_child(
      exmpp_stanza:set_recipient(exmpp_presence:available(), JID),  
      exmpp_xml:element(?NS_MUC, "x")),
  send(Session, Packet),
  list_to_binary(JID).

invite_to_room(Session, Room, Me, Invitee) ->
  Packet = exmpp_xml:append_child(
    exmpp_stanza:set_jids( #xmlel{name = 'message'}, Me, Invitee),  
      exmpp_xml:set_attribute(exmpp_xml:element(?NS_JABBER_X_CONF, x), <<"jid">>, Room)),
  send(Session,Packet).

invite_to_room_mediated(Session, Room, Me, Invitee) ->
  Reason = exmpp_xml:set_cdata(exmpp_xml:element(reason), "Please accept..."),
  Invite = exmpp_xml:append_child(exmpp_stanza:set_recipient(exmpp_xml:element(invite), Invitee), Reason),
  X = exmpp_xml:append_child(exmpp_xml:element(?NS_MUC_USER, x), Invite),
  Message = exmpp_xml:append_child(exmpp_stanza:set_jids(exmpp_xml:element(message), Me, Room), X), 
  send(Session, Message).


handle_packet(_Session, _Me, _Room, UsersOnline, #received_packet{raw_packet=Packet, type_attr="error"})->
  print("ERROR!!!"),
  print(exmpp_xml:document_to_list(Packet)),
  {users, UsersOnline};

handle_packet(Session, Me, _Room, UsersOnline, #received_packet{packet_type=message, raw_packet=Packet, type_attr=Type}) when Type =/= "error" ->
  send_message(Session, Packet, Me, exmpp_stanza:get_sender(Packet), UsersOnline),
  {users, UsersOnline};

handle_packet(_Session, _Me, _Room, UsersOnline, #received_packet{packet_type=iq, raw_packet=Packet}) ->
  io:format("Received IQ stanza:~n~p~n~n", [iq]),
  print(exmpp_xml:document_to_list(Packet)),
  {users, UsersOnline};

handle_packet(Session, Me, Room, UsersOnline, #received_packet{packet_type=presence, raw_packet=Packet}) ->
  Invite = fun(Who) -> invite_to_room(Session, Room, Me, Who), invite_to_room_mediated(Session, Room, Me, Who) end,
  case From = exmpp_stanza:get_sender(Packet) of
    _ when From =/= Room ->
      io:format("Received Presence stanza:~n~p~n~n", [From]),
      send(Session, exmpp_client_disco:info(From)),
      Invite(From),
      {users, [From | UsersOnline]};
    _ when From == Me ->
      {users, UsersOnline};
    _ ->
      lists:foreach(Invite, UsersOnline),
      {users, UsersOnline}
  end;

handle_packet(_Session, _Me, _Room, UsersOnline, #received_packet{raw_packet=Packet}) ->
  print("Unknow type of packet"),
  print(Packet),
  {users, UsersOnline}.


send_message(_, _, _, _, []) -> ok;
send_message(Session, Packet, Me, From, [Me|Others]) -> 
  send_message(Session, Packet, Me, From, Others);
send_message(Session, Packet, Me, From, [From|Others]) -> 
  send_message(Session, Packet, Me, From, Others);
send_message(Session, Packet, Me, From, [To | Others]) -> 
  case exmpp_message:get_body(Packet) of
    undefined -> undefined;
    Body ->
      {jid, _, User, _, _} = exmpp_jid:parse(From),
      send_chat(Session, Me, To, "[" ++ binary_to_list(User) ++ "] " ++ binary_to_list(Body)),
      send_message(Session, Packet, Me, From, Others)
  end.


print(What) ->
   io:format("Debug info:~n~p~n~n", [What]).

send_chat(Session, From, [To], Body ) ->
  send_chat(Session, From, exmpp_jid:parse(To), Body);

send_chat(Session, From, To, Body ) ->
  Msg = exmpp_message:chat(Body),
  Packet = exmpp_stanza:set_jids(Msg, From, To),
  send(Session, Packet).

send(Session,Packet) ->
  print(exmpp_xml:document_to_list(Packet)),
  exmpp_session:send_packet(Session, Packet).
