[![Build Status](https://travis-ci.org/mojolingo/mojoauth.erl.svg?branch=develop)](http://travis-ci.org/mojolingo/mojoauth.erl)

# mojoauth

[MojoAuth](http://mojolingo.com/mojoauth) is a set of standard approaches to cross-app authentication based on [Hash-based Message Authentication Codes](http://en.wikipedia.org/wiki/Hash-based_message_authentication_code) (HMAC), inspired by ["A REST API For Access To TURN Services"](http://tools.ietf.org/html/draft-uberti-behave-turn-rest).

## Dependencies

* rebar
* make

## Building

Simply run `make`.

## Usage

```erlang
% Generate a shared secret
2> Secret = mojoauth:create_secret().
<<"eN1lvHK7cXPYFNwmEwZ3QNMAiCC651E5ikuEOj7+k4EMYTXb3XxXo3iBw4ScxqzJ+aH6aDCCe++LPVGRjgfl3Q==">>

% Create temporary credentials
3> Credentials = mojoauth:create_credentials({id, "foobar"}, {secret, Secret}).
[{username,"1412629132:foobar"},
 {password,<<"Q1RegXu0oYtm1UYqxRkegilugeM=">>}]

% Test credentials
4> mojoauth:test_credentials([{username, "1412629132:foobar"}, {password,<<"Q1RegXu0oYtm1UYqxRkegilugeM=">>}], Secret).
"foobar"
5> mojoauth:test_credentials([{username, "1412629132:foobar"}, {password,"wrongpassword"}], Secret).
false

% 1 day later
6> mojoauth:test_credentials([{username, "1412629132:foobar"}, {password,<<"Q1RegXu0oYtm1UYqxRkegilugeM=">>}], Secret).
false
```

## Contributing

1. [Fork it](https://github.com/mojolingo/mojoauth.erl/fork)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request
