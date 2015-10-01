-module('blockchain').

%% API exports
-export([create_wallet/1]).
-export([get_balance/2]).
-export([send/5]).

-define(BLOCKCHAIN_BASE_URL, "https://blockchain.info/ru/api/v2").
-define(MERCHANT_BASE_URL, "https://blockchain.info/ru/merchant").
-define(CONTENT_TYPE, "application/x-www-form-urlencoded").
-define(PASSWORD_LENGTH, 10).
-define(DIVIDE_BY, 100000000).

-type password() :: binary().
-type address() :: binary().
-type amount() :: float().
-type guid() :: binary().

%%====================================================================
%% API functions
%%====================================================================

-spec create_wallet(password()) -> any().
create_wallet(Password) when is_binary(Password) ->
    case size(Password) < ?PASSWORD_LENGTH of
        true -> {error, password_should_be_longer};
        false ->
            Data = [{<<"password">>, Password}],
            request(post, url(create_wallet), Data)
    end.

-spec get_balance(guid(), password()) -> any().
get_balance(Guid, Password) ->
    request(get, url({get_balance, Guid, Password}), []).

-spec send(guid(), password(), address(), address(), amount()) -> any().
send(Guid, Password, From, To, Amount) ->
    request(post, url({send, Guid, Password, From, To, Amount}), []).

%%====================================================================
%% Internal functions
%%====================================================================

request(Method, Url, Data) ->
    UrlList = binary_to_list(Url),
    Result = case Method of
                 get -> httpc:request(UrlList);
                 post ->
                     httpc:request(Method, {UrlList, [], ?CONTENT_TYPE, form_data(Data)}, [], [])
             end,
    {ok, {{_, Status, _}, _Headers, Body}} = Result,
    case Status of
        500 ->
            {error, Body};
        _Any ->
            BodyBin = list_to_binary(Body),
            {ok, jsx:decode(BodyBin)}
    end.

api_code() ->
    {ok, ApiCode} = application:get_env(?MODULE, api_code),
    list_to_binary(ApiCode).

url(create_wallet) ->
    iolist_to_binary([?BLOCKCHAIN_BASE_URL, "/create_wallet"]);
url({get_balance, Guid, Password}) ->
    iolist_to_binary([?MERCHANT_BASE_URL, "/", Guid,
                      "/balance?password=", Password, "&api_code=", api_code()]);
url({send, Guid, Password, From, To, Amount}) ->
    iolist_to_binary([?MERCHANT_BASE_URL, "/", Guid, "/payment",
                      "?password=", Password,
                      "&from=", From, "&to=", To,
                      "&amount=", integer_to_binary(Amount)]).

-spec form_data(list()) -> binary().
form_data(Data) ->
    Data2 = [{<<"api_code">>, api_code()} | Data],
    form_data(Data2, []).

form_data([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
form_data([{Key, Value}], Acc) ->
    form_data([], [<<Key/binary, "=", Value/binary>> | Acc]);
form_data([{Key, Value} | Rest], Acc) ->
    form_data(Rest, [<<Key/binary, "=", Value/binary, "&">> | Acc]).
