-module(mojoauth).
-vsn("0.1.0").

-export([
  create_secret/0,
  create_credentials/1,
  test_credentials/1
]).

create_secret() ->
  base64:encode(crypto:strong_rand_bytes(64)).

create_credentials({secret, Secret}) ->
  [
    {username, Secret},
    {password, Secret}
  ].

test_credentials([{username, Username}, {password, Password}]) ->
  Username == Password.
