-module(mojoauth_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

mock_setup_test_() ->
  [
    {setup,
      fun start/0,
      fun stop/1,
      fun after_expiration_tests_false_test/1},
    {setup,
      fun start/0,
      fun stop/1,
      fun after_custom_expiration_tests_false_test/1},
    {setup,
      fun start/0,
      fun stop/1,
      fun asserted_id_after_expiration_tests_false_test/1},
    {setup,
      fun start/0,
      fun stop/1,
      fun asserted_id_after_custom_expiration_tests_false_test/1}
  ].

start() ->
  application:ensure_all_started(moka),
  moka:start(mojoauth).

stop(Moka) ->
  moka:stop(Moka).

create_secret_test() ->
  Secret = mojoauth:create_secret(),
  88 = byte_size(Secret).

created_credentials_test_true_test() ->
  Secret = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({secret, Secret}),
  {ok,undefined} = mojoauth:test_credentials(Credentials, Secret).

incorrect_password_tests_false_test() ->
  Secret = mojoauth:create_secret(),
  [{username, Username}, _] = mojoauth:create_credentials({secret, Secret}),
  {invalid} = mojoauth:test_credentials([{username, Username}, {password, "foobar"}], Secret).

different_secret_tests_false_test() ->
  Secret1 = mojoauth:create_secret(),
  Secret2 = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({secret, Secret1}),
  {invalid} = mojoauth:test_credentials(Credentials, Secret2).

attempt_extend_expiration_tests_false_test() ->
  Secret = mojoauth:create_secret(),
  [_, {password, Password}] = mojoauth:create_credentials({secret, Secret}),
  {Mega, Secs, _} = now(),
  Timestamp = Mega*1000000 + Secs,
  Username = integer_to_list(Timestamp + 10000),
  {invalid} = mojoauth:test_credentials([{username, Username}, {password, Password}], Secret).

after_expiration_tests_false_test(Moka) ->
  Secret = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({secret, Secret}),
  {Mega, Secs, Micro} = os:timestamp(),
  moka:replace(Moka, os, timestamp,
    fun() ->
      {Mega, Secs + 86400 + 1, Micro}
    end),
  moka:load(Moka),
  [?_assertEqual({expired}, mojoauth:test_credentials(Credentials, Secret))].

after_custom_expiration_tests_false_test(Moka) ->
  Secret = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({id, ""}, {ttl, 200}, {secret, Secret}),
  {Mega, Secs, Micro} = os:timestamp(),
  moka:replace(Moka, os, timestamp,
    fun() ->
      {Mega, Secs + 200 + 1, Micro}
    end),
  moka:load(Moka),
  [?_assertEqual({expired}, mojoauth:test_credentials(Credentials, Secret))].

asserted_id_created_credentials_return_id_test() ->
  Id = "foobar",
  Secret = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({id, Id}, {secret, Secret}),
  {ok,Id} = mojoauth:test_credentials(Credentials, Secret).

asserted_id_incorrect_password_tests_false_test() ->
  Id = "foobar",
  Secret = mojoauth:create_secret(),
  [{username, Username}, _] = mojoauth:create_credentials({id, Id}, {secret, Secret}),
  {invalid} = mojoauth:test_credentials([{username, Username}, {password, "foobar"}], Secret).

asserted_id_different_secret_tests_false_test() ->
  Id = "foobar",
  Secret1 = mojoauth:create_secret(),
  Secret2 = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({id, Id}, {secret, Secret1}),
  {invalid} = mojoauth:test_credentials(Credentials, Secret2).

asserted_id_attempt_extend_expiration_tests_false_test() ->
  Id = "foobar",
  Secret = mojoauth:create_secret(),
  [_, {password, Password}] = mojoauth:create_credentials({id, Id}, {secret, Secret}),
  {Mega, Secs, _} = now(),
  Timestamp = Mega*1000000 + Secs,
  Username = string:join([integer_to_list(Timestamp + 10000), Id], ":"),
  {invalid} = mojoauth:test_credentials([{username, Username}, {password, Password}], Secret).

asserted_id_after_expiration_tests_false_test(Moka) ->
  Id = "foobar",
  Secret = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({id, Id}, {secret, Secret}),
  {Mega, Secs, Micro} = os:timestamp(),
  moka:replace(Moka, os, timestamp,
    fun() ->
      {Mega, Secs + 86400 + 1, Micro}
    end),
  moka:load(Moka),
  [?_assertEqual({expired}, mojoauth:test_credentials(Credentials, Secret))].

asserted_id_after_custom_expiration_tests_false_test(Moka) ->
  Id = "foobar",
  Secret = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({id, Id}, {ttl, 200}, {secret, Secret}),
  {Mega, Secs, Micro} = os:timestamp(),
  moka:replace(Moka, os, timestamp,
    fun() ->
      {Mega, Secs + 200 + 1, Micro}
    end),
  moka:load(Moka),
  [?_assertEqual({expired}, mojoauth:test_credentials(Credentials, Secret))].
