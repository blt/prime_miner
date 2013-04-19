# prime_miner

This is an ongoing experiment in making Erlang/OTP no-downtime releases
trivially and automatically. It's not quite working. Also, this project is still
a bit hacky and odd.

The prime-miner is something of a joke. Look at
`apps/prime_miner/prime_miner.erl` to understand why.

## The Current Problem

This experiment is a success if we can:

  * start a detached node that contains some volatile yet valuable state
    (a list of primes, here)
  * make changes to the codebase
  * create a new release based on these changes
  * upgrade the running detached node to the new release without losing our
    volatile state

### Starting a Detached Node

```
> make clean
==> frontman (clean)
==> prime_miner (clean)
==> primes (clean)
==> rel (clean)
==> primes (clean)

> make release
==> frontman (get-deps)
==> prime_miner (get-deps)
==> primes (get-deps)
==> rel (get-deps)
==> primes (get-deps)
==> frontman (compile)
==> prime_miner (compile)
Compiled src/prime_miner_app.erl
Compiled src/prime_miner_sup.erl
Compiled src/prime_miner.erl
Compiled src/pm.erl
==> primes (compile)
Compiled src/primes_sup.erl
Compiled src/primes_app.erl
==> rel (compile)
==> primes (compile)
./bin/release_maker
creating a base release
==> rel (generate)

> make node
./rel/primenode/bin/primenode start &> /dev/null || true

> ./rel/primenode/bin/primenode ping
pong
```

We should now be running `primenode` version one. To confirm:

```
> make repl
./rel/primenode/bin/primenode start &> /dev/null || true
erl -name test -setcookie primenode -remsh primenode@127.0.0.1
Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe]
[kernel-poll:false]

Eshell V5.9.2  (abort with ^G)
(primenode@127.0.0.1)1> release_handler:which_releases().
[{"primenode","1",
  ["kernel-2.15.2","stdlib-1.18.2","sasl-2.2.1",
   "prime_miner-1","frontman-","primes-1","compiler-4.8.2",
   "crypto-2.2","hipe-3.9.2","tools-2.6.8"],
  permanent}]
(primenode@127.0.0.1)2>
```

### Make Changes in the Codebase

If you inspect `apps/prime_miner/src/prime_miner.erl` you'll find it's already
hardwired to modify the state transition timeout that drives the prime mining
loop. All we have to do to effect an appropriate code change is toggle some
version numbers up. The diff:

```
> git diff
diff --git a/apps/prime_miner/src/prime_miner.app.src b/apps/prime_miner/src/prime_miner
index dbec34c..47533e6 100644
--- a/apps/prime_miner/src/prime_miner.app.src
+++ b/apps/prime_miner/src/prime_miner.app.src
@@ -1,7 +1,7 @@
 {application, prime_miner,
  [
   {description, "Discovers primes. Worth money?"},
-  {vsn, "1"},
+  {vsn, "2"},
   {modules, [pm]},
   {registered, []},
   {applications, [
diff --git a/rel/reltool.config b/rel/reltool.config
index abd853b..722fd48 100644
--- a/rel/reltool.config
+++ b/rel/reltool.config
@@ -2,7 +2,7 @@
        {lib_dirs, ["../apps"]},
        {erts, [{mod_cond, derived}, {app_file, strip}]},
        {app_file, strip},
-       {rel, "primenode", "1",
+       {rel, "primenode", "2",
         [
          kernel,
          stdlib,
```

and a new release:

```
> make release
==> frontman (get-deps)
==> prime_miner (get-deps)
==> primes (get-deps)
==> rel (get-deps)
==> primes (get-deps)
==> frontman (compile)
==> prime_miner (compile)
==> primes (compile)
==> rel (compile)
==> primes (compile)
./bin/release_maker
base release already created; time to update
==> rel (generate)
==> rel (generate-upgrade)
primenode_2 upgrade package created
```

### Create a New Release Based on These Changes

The Makefile uses `bin/upgrade.escript` to perform the usual release
handler dance. First, confirm that the needed release tarball is in
place:

```
> ls rel/primenode/releases
1                  RELEASES           primenode_2.tar.gz start_erl.data
```

Now, the escript:

```
> escript bin/upgrade.escript primenode@127.0.0.1 primenode primenode_2
escript: exception error: no match of right hand side value
                 {error,
                     {{badmatch,{error,enoent}},
                      [{erl_tar,default_options,0,
                           [{file,"erl_tar.erl"},{line,440}]},
                       {erl_tar,extract_opts,1,
                           [{file,"erl_tar.erl"},{line,434}]},
                       {erl_tar,extract,2,[{file,"erl_tar.erl"},{line,136}]},
                       {release_handler,do_unpack_release,4,
                           [{file,"release_handler.erl"},{line,834}]},
                       {release_handler,handle_call,3,
                           [{file,"release_handler.erl"},{line,588}]},
                       {gen_server,handle_msg,5,
                           [{file,"gen_server.erl"},{line,588}]},
                       {proc_lib,init_p_do_apply,3,
                           [{file,"proc_lib.erl"},{line,227}]}]}}
```

If I attach directly to the node and manually drive the release:

```
> make repl
./rel/primenode/bin/primenode start &> /dev/null || true
erl -name test -setcookie primenode -remsh primenode@127.0.0.1
Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.2  (abort with ^G)
(primenode@127.0.0.1)1> TargetNode = 'primenode@127.0.0.1'.
'primenode@127.0.0.1'
(primenode@127.0.0.1)2>
(primenode@127.0.0.1)2> Cookie = primenode.
primenode
(primenode@127.0.0.1)3>
(primenode@127.0.0.1)3> ReleasePackage = "primenode_2".
"primenode_2"
(primenode@127.0.0.1)4> rpc:call(TargetNode, release_handler, unpack_release, [ReleasePackage], 6000).
{error,{{badmatch,{error,enoent}},
        [{erl_tar,default_options,0,
                  [{file,"erl_tar.erl"},{line,440}]},
         {erl_tar,extract_opts,1,[{file,"erl_tar.erl"},{line,434}]},
         {erl_tar,extract,2,[{file,"erl_tar.erl"},{line,136}]},
         {release_handler,do_unpack_release,4,
                          [{file,"release_handler.erl"},{line,834}]},
         {release_handler,handle_call,3,
                          [{file,"release_handler.erl"},{line,588}]},
         {gen_server,handle_msg,5,
                     [{file,"gen_server.erl"},{line,588}]},
         {proc_lib,init_p_do_apply,3,
                   [{file,"proc_lib.erl"},{line,227}]}]}}
(primenode@127.0.0.1)5> release_handler:unpack_release(ReleasePackage).
{error,{{badmatch,{error,enoent}},
        [{erl_tar,default_options,0,
                  [{file,"erl_tar.erl"},{line,440}]},
         {erl_tar,extract_opts,1,[{file,"erl_tar.erl"},{line,434}]},
         {erl_tar,extract,2,[{file,"erl_tar.erl"},{line,136}]},
         {release_handler,do_unpack_release,4,
                          [{file,"release_handler.erl"},{line,834}]},
         {release_handler,handle_call,3,
                          [{file,"release_handler.erl"},{line,588}]},
         {gen_server,handle_msg,5,
                     [{file,"gen_server.erl"},{line,588}]},
         {proc_lib,init_p_do_apply,3,
                   [{file,"proc_lib.erl"},{line,227}]}]}}
(primenode@127.0.0.1)6> release_handler:unpack_release("primenode_2.tar.gz").
{error,{no_such_file,"/Users/blt/projects/us/troutwine/primes/rel/primenode/releases/primenode_2.tar.gz.tar.gz"}}
(primenode@127.0.0.1)7> release_handler:unpack_release("primenode_2").
{error,{{badmatch,{error,enoent}},
        [{erl_tar,default_options,0,
                  [{file,"erl_tar.erl"},{line,440}]},
         {erl_tar,extract_opts,1,[{file,"erl_tar.erl"},{line,434}]},
         {erl_tar,extract,2,[{file,"erl_tar.erl"},{line,136}]},
         {release_handler,do_unpack_release,4,
                          [{file,"release_handler.erl"},{line,834}]},
         {release_handler,handle_call,3,
                          [{file,"release_handler.erl"},{line,588}]},
         {gen_server,handle_msg,5,
                     [{file,"gen_server.erl"},{line,588}]},
         {proc_lib,init_p_do_apply,3,
                   [{file,"proc_lib.erl"},{line,227}]}]}}
```

Perhaps there's something wrong with the release generation?