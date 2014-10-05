-module(mojoauth).
-vsn("0.1.0").

-export([
  create_secret/0,
  create_credentials/1,
  create_credentials/2,
  test_credentials/2
]).

create_secret() ->
  base64:encode(crypto:strong_rand_bytes(64)).

create_credentials({id, Id}, {secret, Secret}) ->
  {Mega, Secs, _} = os:timestamp(),
  Timestamp = Mega*1000000 + Secs + 86400,
  Username = string:join([integer_to_list(Timestamp), Id], ":"),
  [
    {username, Username},
    {password, sign(Username, Secret)}
  ].

create_credentials({secret, Secret}) ->
  create_credentials({id, ""}, {secret, Secret}).

test_credentials([{username, Username}, {password, Password}], Secret) ->
  RetVal = case string:tokens(Username, ":") of
    [Timestamp, Id] ->
      case still_valid(Timestamp) of
        true -> Id;
        false -> false
      end;
    [Timestamp] ->
      still_valid(Timestamp)
  end,
  test_signature(Username, Password, Secret, RetVal).

sign(Message, Secret) ->
  base64:encode(crypto:hmac(sha, Secret, Message)).

test_signature(Username, Password, Secret, RetVal) ->
  case sign(Username, Secret) of
    Password -> RetVal;
    _ -> false
  end.

still_valid(Timestamp) ->
  {Mega, Secs, _} = os:timestamp(),
  Now = Mega*1000000 + Secs,
  Now < list_to_integer(Timestamp).
