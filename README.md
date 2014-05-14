NAME
====

influxdb_cli

SYNOPSIS
========

```sh
  % rebar get-deps compile
  % erl -pa ../influxdb_cli/ebin -pa deps/*/ebin
```
```erlang
  > inets:start().
  ok
  > application:start(influxdb_cli).
  ok
  > influxdb_cli_client:usage().
```

DESCRIPTION
===========

This application provides command line interface for InfluxDB with erlang shell.

InfluxDB has no command line interface, but many languages provides command line
interface.  This is one of these interfaces.
