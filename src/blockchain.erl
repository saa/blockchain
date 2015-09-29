-module('blockchain').

%% API exports
-export([create_wallet/2]).

-define(BLOCKCHAIN_BASE_URL, "https://blockchain.info/ru/api/v2").
-define(CONTENT_TYPE, "application/x-www-form-urlencoded").
-define(PASSWORD_LENGTH, 10).

-type action() :: create_wallet.

%%====================================================================
%% API functions
%%====================================================================

-spec create_wallet(binary(), binary()) -> any().
create_wallet(Password, ApiCode) when is_binary(Password),
                                      is_binary(ApiCode) ->
    case size(Password) < ?PASSWORD_LENGTH of
        true -> {error, password_should_be_longer};
        false ->
            Data = [{<<"password">>, Password}, {<<"api_code">>, ApiCode}],
            request(create_wallet, Data)
    end.

request(create_wallet, Data) ->
    request(post, url(create_wallet), Data).

request(post, Url, Data) ->
    {ok, {{_, Status, _}, _Headers, Body}} = do_post(Url, Data),
    case Status of
        500 ->
            {error, Body};
        _Any ->
            {ok, Body}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec url(action()) -> binary().
url(create_wallet) ->
    iolist_to_binary([?BLOCKCHAIN_BASE_URL, "/create_wallet"]).

-spec do_post(binary(), list()) -> any().
do_post(Url, Data) when is_binary(Url) ->
    UrlList = binary_to_list(Url),
    httpc:request(post, {UrlList, [], ?CONTENT_TYPE, form_data(Data)}, [], []).

-spec form_data(list()) -> binary().
form_data(Data) ->
    form_data(Data, []).

form_data([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
form_data([{Key, Value}], Acc) ->
    form_data([], [<<Key/binary, "=", Value/binary>> | Acc]);
form_data([{Key, Value} | Rest], Acc) ->
    form_data(Rest, [<<Key/binary, "=", Value/binary, "&">> | Acc]).

