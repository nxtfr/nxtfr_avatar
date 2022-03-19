-module(nxtfr_avatar).
-author("christian@flodihn.se").
-behaviour(gen_server).

%% External exports
-export([
    start_link/0,
    create/1,
    create/2,
    read/1,
    update/1,
    delete/1,
    logical_delete/1,
    restore/1]).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% server state
-record(state, {storage_module, storage_state}).

-type state() :: #state{storage_module :: atom, storage_state :: any()}.
-type avatar() :: #{
    uid => binary(),
    account_uid => binary(),
    created_at => binary(),
    updated_at => binary() | undefined,
    deleted_at => binary() | undefined,
    restored_at => binary() | undefined}.

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec create(AccountUid :: binary()) -> {ok, Avatar :: avatar()} | {error, avatar_already_exists}.
create(AccountUid) ->
    gen_server:call(?MODULE, {create, #{}, AccountUid}).

-spec create(UserMap :: map(), AccountUid :: binary()) -> {ok, Avatar :: avatar()} | {error, Reason :: atom()}.
create(UserMap, AccountUid) ->
    gen_server:call(?MODULE, {create, UserMap, AccountUid}).

-spec read(Uid :: binary()) -> {ok, Avatar :: avatar()} | {error, Reason :: atom()}.
read(Uid) ->
    gen_server:call(?MODULE, {read, Uid}).

-spec update(Avatar :: avatar()) -> {ok, updated} | {error, Reason :: atom()}.
update(Avatar) ->
    gen_server:call(?MODULE, {update, Avatar}).

-spec delete(Uid :: binary()) -> {ok, deleted} | {error, Reason :: atom()}.
delete(Avatar) ->
    gen_server:call(?MODULE, {delete, Avatar}).

-spec logical_delete(Uid :: binary()) -> {ok, deleted} | {error, Reason :: atom()}.
logical_delete(Avatar) ->
    gen_server:call(?MODULE, {logical_delete, Avatar}).

-spec restore(Uid :: binary()) -> {ok, avatar_restored} | {error, avatar_not_found}.
restore(Uid) ->
    gen_server:call(?MODULE, {restore, Uid}).

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, StorageModule} = application:get_env(storage_module),
    {ok, AutoDiscoveryGroup} = application:get_env(autodiscovery_group),
    nxtfr_event:notify({join_autodiscovery_group, AutoDiscoveryGroup}),
    {ok, StorageState} = StorageModule:init(),
    {ok, #state{
        storage_module = StorageModule,
        storage_state = StorageState}}.

handle_call(
        {create, UserMap, AccountUid},
        _From,
        #state{storage_module = StorageModule, storage_state = StorageState} = State) ->
    #{uid := Uid} = Avatar = create_avatar(UserMap, AccountUid),
    case StorageModule:read(Uid, StorageState) of
        {ok, _ExistingAvatar} ->
            {reply, {error, avatar_already_exists}, State};
        not_found ->
            {ok, created} = StorageModule:create(Avatar, StorageState),
            {reply, {ok, Avatar}, State}
    end;

handle_call(
        {read, Uid},
        _From,
        #state{storage_module = StorageModule, storage_state = StorageState} = State) ->
    case StorageModule:read(Uid, StorageState) of
        {ok, #{deleted := true}} ->
            {reply, {error, avatar_not_found}, State};
        {ok, Avatar} ->
            {reply, {ok, Avatar}, State};
        not_found ->
            {reply, {error, avatar_not_found}, State}
    end;

handle_call(
        {update, Avatar},
        _From,
        #state{storage_module = StorageModule, storage_state = StorageState} = State) ->
    case StorageModule:update(Avatar#{updated_at => get_rfc3339_time()}, StorageState) of
        {ok, updated} -> {reply, {ok, avatar_updated}, State};
        {error, not_found} -> {reply, {error, avatar_not_found}, State}
    end;

handle_call(
        {delete, Uid},
        _From,
        #state{storage_module = StorageModule, storage_state = StorageState} = State) ->
    case StorageModule:delete(Uid, StorageState) of
        {error, not_found} ->
            {reply, {error, avatar_not_found}, State};
        {ok, deleted} ->
            {reply, {ok, avatar_deleted}, State}
    end;

handle_call(
        {logical_delete, Uid},
        _From,
        #state{storage_module = StorageModule, storage_state = StorageState} = State) ->
    case StorageModule:read(Uid, StorageState) of
        {ok, Avatar} ->
            DeletedAvatar = Avatar#{deleted => true, deleted_at => get_rfc3339_time()},
            {ok, updated} = StorageModule:update(DeletedAvatar, StorageState),
            {reply, {ok, avatar_deleted}, State};
        not_found ->
            {reply, {error, avatar_not_found}, State}
    end;

handle_call(
        {restore, Uid},
        _From,
        #state{storage_module = StorageModule, storage_state = StorageState} = State) ->
    case StorageModule:read(Uid, StorageState) of
        {ok, Avatar} ->
            DeletedAvatar = Avatar#{deleted => false, restored_at => get_rfc3339_time()},
            {ok, updated} = StorageModule:update(DeletedAvatar, StorageState),
            {reply, {ok, avatar_restored}, State};
        not_found ->
            {reply, {error, avatar_not_found}, State}
    end;


handle_call(Call, _From, State) ->
    error_logger:error_report([{undefined_call, Call}]),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    error_logger:error_report([{undefined_cast, Cast}]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_report([{undefined_info, Info}]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

create_avatar(Avatar, AccountUid) ->
    Avatar#{
        uid => make_uid(),
        deleted => false,
        account_uid => AccountUid,
        created_at => get_rfc3339_time(),
        updated_at => undefined,
        deleted_at => undefined,
        restored_at => undefined}.

%% @doc Implementation taken from https://github.com/afiskon/erlang-uuid-v4/blob/master/src/uuid.erl
make_uid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    Str = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b", 
                        [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
    list_to_binary(Str).

get_rfc3339_time() ->
    list_to_binary(calendar:system_time_to_rfc3339(os:system_time(second))).