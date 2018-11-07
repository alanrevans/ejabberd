-module(mod_sms).

-behaviour(gen_server).

-behaviour(gen_mod).

%% gen_mod API
-export([start/2, stop/1, reload/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([depends/2, mod_options/1, in_subscription/2]).

-include("logger.hrl").
-include("xmpp.hrl").
-include("mod_roster.hrl").

-record(state, {
                 host = <<"">>
               }).

%%====================================================================
%% API
%%====================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, _OldOpts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {set_state, parse_options(Host, NewOpts)}).

depends(_Host, _Opts) ->
    [{mod_roster, hard}].

mod_options(_Host) ->
    [].


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    process_flag(trap_exit, true),
    State = parse_options(Host, Opts),
    ejabberd_hooks:add(roster_in_subscription, Host,
                       ?MODULE, in_subscription, 5),
    {ok, State}.

parse_options(Host, _Opts) ->
    #state{host = Host}.


handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({set_state, NewState}, _State) ->
    {noreply, NewState};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(roster_in_subscription, Host,
                          ?MODULE, in_subscription, 30).

code_change(_OldVsn, State, _Extra) -> {ok, State}.


-spec in_subscription(boolean(), presence()) -> boolean().
in_subscription(Acc, #presence{to = To, from = JID, type = Type}) ->
    #jid{user = User, server = Server} = To,
    process_subscription(in, User, Server, JID, Type, Acc).


process_subscription(in, User, Server, JID,
                     _Type, Acc) ->
    case ejabberd_auth:user_exists(User, Server) of
      true -> Acc;
      false -> 
          ?INFO_MSG("Send an SMS to ~p", [User]),
          process_subscription_in(JID, User, Server),
          Acc
    end.

process_subscription_in(JID, LUser, LServer) ->
    Item = get_roster_item(LUser, LServer, JID),
    RosterItem = Item#roster{subscription = none,
                             ask = in,
                             askmessage = <<>>},
    roster_subscribe_t(LUser, LServer, JID, RosterItem).
    
roster_subscribe_t(LUser, LServer, JID, Item) ->
    Mod = gen_mod:db_mod(LServer, mod_roster),
    transaction(
      LUser, LServer, [JID],
      fun() ->
              Mod:roster_subscribe(LUser, LServer, JID, Item),
              send_sms(JID, LUser)
      end).

get_roster_item(LUser, LServer, RJID) ->
    #jid{luser = RUser,lserver = RServer} = RJID,
    Mod = gen_mod:db_mod(LServer, mod_roster),
    Result = transaction(
      LUser, LServer, [{RUser, RServer, <<>>}],
      fun() ->
              Mod:get_roster_item(LUser, LServer, {RUser, RServer, <<>>})
      end),
    case Result of
        {ok, Item} ->
            Item;
        error ->
            #roster{usj = {LUser, LServer, {RUser, RServer, <<>>}},
                    us = {LUser, LServer}, 
                    name = mod_admin_extra:get_vcard(RUser, RServer, <<"FN">>),
                    jid = {RUser, RServer, <<>>}}
    end.

send_sms(JID, To) ->
    #jid{luser = RUser,lserver = RServer} = JID,
    Family = mod_admin_extra:get_vcard(RUser, RServer, <<"N">>, <<"FAMILY">>),
    Given = mod_admin_extra:get_vcard(RUser, RServer, <<"N">>, <<"GIVEN">>),
    Msg = <<Given/binary, " ", Family/binary, " (",RUser/binary, ") is trying to reach you via num.im, goto https://chat.num.im/xmpp_onboard/ldap_users/register/", To/binary, " to register an account">>,
    Data = {[{<<"smsmsc">>, <<"+33644402000">>}, 
             {<<"smsmsg">>, Msg}, 
             {<<"from">>, <<"208220001026736">>}, 
             {<<"to">>, <<"+", To/binary>>},
             {<<"Msg-ID">>,<<"randomstuff">>}
            ]
           }, 
    Body = jiffy:encode(Data),
    ?INFO_MSG("SMS body ~p", [Body]),
    p1_http:request(put, "https://alanrevans:upupandaway@ss7if.kagesys.com:48088/smsmsc/208220001026736/send_sms", [], Body, []).


transaction(LUser, LServer, LJIDs, F) ->
    Mod = gen_mod:db_mod(LServer, mod_roster),
    case Mod:transaction(LServer, F) of
        {atomic, error} -> error;
        {atomic, {ok, _} = Result} ->
%%            delete_cache(Mod, LUser, LServer, LJIDs),
            Result;
        Err ->
            Err
    end.

