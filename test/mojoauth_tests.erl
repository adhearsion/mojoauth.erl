-module(mojoauth_tests).

-compile(export_all).

create_secret_test() ->
  Secret = mojoauth:create_secret(),
  88 = byte_size(Secret).

created_credentials_test_true_test() ->
  Credentials = mojoauth:create_credentials({secret, mojoauth:create_secret()}),
  true = mojoauth:test_credentials(Credentials).
