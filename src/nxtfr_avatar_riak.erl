-module(nxtfr_avatar_riak).
-author("christian@flodihn.se").

-define(AVATARS_TABLE, <<"avatars">>).

-record(riak_state, {riak_client_pid :: pid()}).

-type riak_state() :: #riak_state{}.

-export([
    init/0,
    stop/1,
    create/2,
    read/2,
    update/2,
    delete/2]).

-spec init() -> {ok, RiakState :: riak_state()}.
init() ->
    %% In case the supervisor trigger restarts because of lost db connection
    %% or similar. We want to avoid restarting too quickly.
    timer:sleep(500),
    {ok, RiakOptions} = application:get_env(riak_options),
    Hostname = proplists:get_value(hostname, RiakOptions, "127.0.0.1"),
    Port = proplists:get_value(port, RiakOptions, 8087),
    {ok, Pid} = riakc_pb_socket:start(Hostname, Port),
    {ok, #riak_state{riak_client_pid = Pid}}.

-spec stop(RiakState :: riak_state()) -> ok.
stop(#riak_state{riak_client_pid = Pid}) ->
    riakc_pb_socket:stop(Pid).

-spec create(Avatar :: map(), RiakState :: riak_state()) -> {ok, saved}.    
create(#{uid := Uid, account_uid := AccountUid} = Avatar, #riak_state{riak_client_pid = Pid}) ->
    AvatarObject = riakc_obj:new(?AVATARS_TABLE, Uid, term_to_binary(Avatar)),
    AvatarObjectMetaData = riakc_obj:get_update_metadata(AvatarObject),
    AvatarObjectMetaData2 = riakc_obj:set_secondary_index(AvatarObjectMetaData, [{{binary_index, "account_uid"}, [AccountUid]}]),
    AvatarObjectWithIndex = riakc_obj:update_metadata(AvatarObject, AvatarObjectMetaData2),
    riakc_pb_socket:put(Pid, AvatarObjectWithIndex, [{w, 1}, {dw, 1}]),
    {ok, created}.

-spec read(Uid :: binary, RiakState :: riak_state()) -> {ok, Account :: map()} | not_found.
read(Uid, #riak_state{riak_client_pid = Pid}) ->
    case riakc_pb_socket:get(Pid, ?AVATARS_TABLE, Uid) of
        {error, notfound}->
            not_found;
        {ok, AvatarObject} ->
            {ok, binary_to_term(riakc_obj:get_value(AvatarObject))}    
    end.

-spec update(Avatar :: map(), RiakState :: riak_state()) -> {ok, updated} | not_found.
update(#{uid := Uid} = Avatar, #riak_state{riak_client_pid = Pid}) ->
    case riakc_pb_socket:get(Pid, ?AVATARS_TABLE, Uid) of
        {error, notfound} ->
            not_found;
        {ok, AvatarObject} ->
            UpdatedAvatarObject = riakc_obj:update_value(AvatarObject, term_to_binary(Avatar)),
            ok = riakc_pb_socket:put(Pid, UpdatedAvatarObject, [{w, 1}, {dw, 1}]),
            {ok, updated}
    end.

-spec delete(Uid :: binary, RiakState :: riak_state()) -> {ok, deleted} | not_found.
delete(Uid, #riak_state{riak_client_pid = Pid}) ->
    riakc_pb_socket:delete(Pid, ?AVATARS_TABLE, Uid),
    {ok, deleted}.