-module(mojoauth_tests).

-compile(export_all).

create_secret_test() ->
  Secret = mojoauth:create_secret(),
  88 = byte_size(Secret).

created_credentials_test_true_test() ->
  Secret = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({secret, Secret}),
  true = mojoauth:test_credentials(Credentials, Secret).

incorrect_password_tests_false_test() ->
  Secret = mojoauth:create_secret(),
  [{username, Username}, _] = mojoauth:create_credentials({secret, Secret}),
  false = mojoauth:test_credentials([{username, Username}, {password, "foobar"}], Secret).

different_secret_tests_false_test() ->
  Secret1 = mojoauth:create_secret(),
  Secret2 = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({secret, Secret1}),
  false = mojoauth:test_credentials(Credentials, Secret2).

asserted_id_created_credentials_return_id_test() ->
  Id = "foobar",
  Secret = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({id, Id}, {secret, Secret}),
  Id = mojoauth:test_credentials(Credentials, Secret).

asserted_id_incorrect_password_tests_false_test() ->
  Id = "foobar",
  Secret = mojoauth:create_secret(),
  [{username, Username}, _] = mojoauth:create_credentials({id, Id}, {secret, Secret}),
  false = mojoauth:test_credentials([{username, Username}, {password, "foobar"}], Secret).

asserted_id_different_secret_tests_false_test() ->
  Id = "foobar",
  Secret1 = mojoauth:create_secret(),
  Secret2 = mojoauth:create_secret(),
  Credentials = mojoauth:create_credentials({id, Id}, {secret, Secret1}),
  false = mojoauth:test_credentials(Credentials, Secret2).
