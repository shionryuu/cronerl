
# conerl - cron service

[![Build Status](https://secure.travis-ci.org/ShionRyuu/conerl.png?branch=master)](https://travis-ci.org/ShionRyuu/conerl)

Cron service implementation in Erlang.

## Examples

```erlang
application:start(crontab).

crontab_server:add_crontab(1, {'*', "*", '*', '*', '*'}, {io, fmt, ["triggered~n"]}, []).

crontab_server:add_crontab(2, {'*', "*", '*', '*', '*'}, {io, format, ["triggered~n"]}, []).

```

## Authors

- Shion Ryuu <shionryuu@outlook.com>

## Contributors

- Paul Vixie <paul@vix.com>

## License

conerl is available under the [MIT License](http://shionryuu.mit-license.org/).



