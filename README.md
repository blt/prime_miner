# prime_miner

This is an ongoing experiment in making Erlang/OTP no-downtime releases
trivially and automatically. It's working within the confines of updating from
base v1 to v2 only. This project is still a bit hacky and odd.

The prime-miner is something of a joke. Look at
`apps/prime_miner/prime_miner.erl` to understand why.

## The Aim

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
==> prime_miner (clean)
==> primes (clean)
==> rel (clean)
==> primes (clean)

> make release
==> prime_miner (get-deps)
==> primes (get-deps)
==> rel (get-deps)
==> primes (get-deps)
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
   "prime_miner-1","primes-1","compiler-4.8.2",
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

### Create a New Release Based on These Changes

Rather than rely on rebar's automatic generation of appup files, `prime_miner`
defines its own. I found it to be very difficult to reliably generate appup
files with rebar that did the Right Thing always. To create a new release:

```
> make release
==> prime_miner (get-deps)
==> primes (get-deps)
==> rel (get-deps)
==> primes (get-deps)
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

The Makefile uses `bin/upgrade.escript` to perform the usual release
handler dance. First, confirm that the needed release tarball is in
place:

```
> ls rel/primenode/releases
1                  RELEASES           primenode_2.tar.gz start_erl.data
```

Now, run the update:

```
> make update
./rel/primenode/bin/primenode start &> /dev/null || true
escript bin/upgrade.escript primenode@127.0.0.1 primenode primenode_2
Unpacked Release "2"
Installed Release "2"
Made Release "2" Permanent
```

If we `make repl` as before we can request the release list to show that we are
now on primenode v2.