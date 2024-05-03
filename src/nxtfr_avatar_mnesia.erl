-module(nxtfr_avatar_mnesia).
-author("christian@flodihn.se").

-define(AVATARS_TABLE, avatars).

-record(avatar, {
    uid :: binary(),
    account_uid :: binary(),
    map :: map()}).

-export([
    init/0,
    stop/1,
    create/2,
    read/2,
    update/2,
    delete/2]).

-spec init() -> {ok, []}.
init() ->
    mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    case lists:member(?AVATARS_TABLE, mnesia:system_info(tables)) of
        true ->
            pass;
        false ->
            mnesia:create_table(?AVATARS_TABLE, [
                {record_name, avatar},
                {attributes, record_info(fields, avatar)},
                {disc_only_copies, [node()]}])
    end,
    {ok, []}.

-spec stop(MnesiaState :: []) -> ok.
stop(_MnesiaState) ->
    ok.

-spec create(AvatarMap :: map(), MnesiaState :: []) -> {ok, created} | {error, Reason :: any}.
create(#{uid := Uid, account_uid := AccountUid} = AvatarMap, _MnesiaState) ->
    Record = #avatar{uid = Uid, account_uid = AccountUid, map = AvatarMap},
    case write(?AVATARS_TABLE, Record) of
        {atomic, ok} ->
            {ok, created};
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec read(Uid :: binary, MnesiaState :: []) -> {ok, AvatarMap :: map()} | not_found.
read(Uid, _MnesiaState) ->
   case read({?AVATARS_TABLE, Uid}) of 
        {atomic, [AvatarRecord]} -> {ok, AvatarRecord#avatar.map};
        {atomic, []} -> not_found
    end.

-spec update(AvatarMap :: map(), MnesiaState :: []) -> {ok, updated} | not_found | {error, Reason :: any}.
update(#{uid := Uid} = AvatarMap, _MnesiaState) ->
    case read({?AVATARS_TABLE, Uid}) of 
        {atomic, [#avatar{uid = Uid} = Record]} ->
            case write(?AVATARS_TABLE, Record#avatar{map = AvatarMap}) of
                {atomic, ok} ->
                    {ok, updated};
                {aborted, Reason} ->
                    {error, Reason}
            end;
        {atomic, []} ->
            not_found
    end.

-spec delete(Uid :: binary, MnesiaState :: []) -> {ok, deleted} | not_found | {error, Reason :: any}.
delete(Uid, _MnesiaState) ->
    case read({?AVATARS_TABLE, Uid}) of 
        {atomic, [#avatar{uid = Uid}]} ->
            case delete({?AVATARS_TABLE, Uid}) of 
                {atomic, ok} -> {ok, deleted};
                {aborted, Reason} -> {error, Reason}
            end;
        {atomic, []} ->
            not_found
    end.

write(Table, Record) ->
    F = fun() ->
        mnesia:write(Table, Record, write)
    end,
    mnesia:transaction(F).
    
read(Q) ->
    F = fun() ->
        mnesia:read(Q)
    end,
    mnesia:transaction(F).
    
delete(Q) ->
    F = fun() ->
        mnesia:delete(Q)
    end,
    mnesia:transaction(F).