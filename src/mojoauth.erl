%% @type id(Id) = {id, Id}
%%      Id = string()
%% @type ttl(Ttl) = {ttl, Ttl}
%%      Ttl = integer()
%% @type secret(Secret) = {secret, Secret}
%%      Secret = string()
%% @type username(Username) = {username, Username}
%%      Username = string()
%% @type password(Password) = {password, Password}
%%      Password = string()
%% @type credential(Username, Password) = list(username(), password())

-module(mojoauth).
-vsn("0.1.0").

-export([
  create_secret/0,
  create_credentials/1,
  create_credentials/2,
  create_credentials/3,
  test_credentials/2
]).

%% @spec create_secret() -> string()
%% @doc Create a new random secret
create_secret() ->
  base64:encode(crypto:strong_rand_bytes(64)).

%% @spec create_credentials(Id::id(), Ttl::ttl(), Secret::secret()) -> credential()
%% @doc Create a new set of credentials for an asserted ID, given a desired TTL and shared secret
create_credentials({id, Id}, {ttl, Ttl}, {secret, Secret}) ->
  {Mega, Secs, _} = os:timestamp(),
  Timestamp = Mega*1000000 + Secs + Ttl,
  Username = string:join([integer_to_list(Timestamp), Id], ":"),
  [
    {username, list_to_binary(Username)},
    {password, sign(Username, Secret)}
  ].

%% @spec create_credentials(Id::id(), Secret::secret()) -> credential()
%% @doc Create a new set of credentials for an asserted ID, given the default TTL of 1 day and shared secret
create_credentials({id, Id}, {secret, Secret}) ->
  create_credentials({id, Id}, {ttl, 86400}, {secret, Secret}).

%% @spec create_credentials(Secret::secret()) -> credential()
%% @doc Create a new set of credentials for a shared secret without an asserted ID and the default TTL of 1 day
create_credentials({secret, Secret}) ->
  create_credentials({id, ""}, {secret, Secret}).

%% @spec test_credentials(Credential::credential(), secret()) -> any()
%% @doc Test a set of credentials are valid for the given secret. When an identity is asserted, the identity is returned as a string, otherwise a boolean is returned.
test_credentials([{username, Username}, {password, Password}], Secret) ->
  RetVal = case string:tokens(binary_to_list(Username), ":") of
    [Timestamp, Id] -> list_to_binary(Id);
    [Timestamp] -> undefined
  end,
  case still_valid(Timestamp) of
    true -> test_signature(Username, Password, Secret, RetVal);
    false -> {expired}
  end.

sign(Message, Secret) ->
  base64:encode(crypto:hmac(sha, Secret, Message)).

test_signature(Username, Password, Secret, Id) ->
  case sign(Username, Secret) of
    Password -> {ok,Id};
    _ -> {invalid}
  end.

still_valid(Timestamp) ->
  {Mega, Secs, _} = os:timestamp(),
  Now = Mega*1000000 + Secs,
  Now < list_to_integer(Timestamp).
