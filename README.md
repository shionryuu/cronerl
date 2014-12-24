
# cronerl - erlang cron service

[![Build Status](https://secure.travis-ci.org/ShionRyuu/cronerl.png?branch=master)](https://travis-ci.org/ShionRyuu/cronerl)

Cron service implementation in Erlang.

## Examples

```erlang
crontab_app:start().

crontab:add_crontab(1, {'*', "*", '*', '*', '*'}, {io, fmt, ["1 triggered~n"]}).

crontab:add_crontab(2, {'*', "*", '*', '*', '*'}, {io, format, ["2 triggered~n"]}).

crontab:add_crontab(3, {"0-59/20", "22-23", "15-17,20,24", 12, "6,7"}, {io, format, ["3 triggered~n"]}).

```

## Config File

```
{1, {'*', "*", '*', '*', '*'}, {io, fmt, ["1 triggered~n"]}}.
{2, {'*', "*", '*', '*', '*'}, {io, format, ["2 triggered~n"]}}.
```

## Authors

- Shion Ryuu <shionryuu@outlook.com>

## Contributors

- Paul Vixie <paul@vix.com>

## License

conerl is available under the [MIT License](http://shionryuu.mit-license.org/).

