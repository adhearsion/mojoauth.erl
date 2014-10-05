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
  Timestamp = Mega*1000000 + Secs,
  Username = string:join([integer_to_list(Timestamp), Id], ":"),
  [
    {username, Username},
    {password, sign(Username, Secret)}
  ].

create_credentials({secret, Secret}) ->
  create_credentials({id, ""}, {secret, Secret}).

test_credentials([{username, Username}, {password, Password}], Secret) ->
  case string:tokens(Username, ":") of
    [_, Id] ->
      test_signature(Username, Password, Secret, Id);
    [_] ->
      test_signature(Username, Password, Secret, true)
  end.

sign(Message, Secret) ->
  base64:encode(crypto:hmac(sha, Secret, Message)).

test_signature(Username, Password, Secret, RetVal) ->
  case sign(Username, Secret) of
    Password -> RetVal;
    _ -> false
  end.
