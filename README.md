### Running Dependencies

Application |                        Version 
----------- | ------------------------------
cowboy      | tsloughter/client_improvements
gproc       |                         0.2.13

### Testing Dependencies

Application | Version 
----------- | -------
ibrowse     |   3.0.3

### Building

```shell
$ export ERL_LIBS=$ERL_LIBS:./deps
$ ./sinan dist
```

### Running

```shell
$ _build/dwight/bin/dwight -config config/sys.config
```
### Testing

The configuration is currently as follows

```erlang
[ {dwight_routes, [{domains, [{<<"localhost">>, <<"abc">>}]},
                   {route_ids, [{<<"abc">>, <<"localhost">>, 7999}]}]} ].

```

So if you have a local web server on 7999 you can use either curl or
dwight_test:

```shell
$ curl -v localhost:8080
```

```erlang
1> dwight_test:run().
```
