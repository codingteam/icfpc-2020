# Codingteam's submission for ICFP Programming Contest 2020

## Building locally inside organizer's Docker images

To get the exact same environment as the organizers are using, one has to build
our code inside [Docker](https://www.docker.com/). In the root of the repo, run:

```console
$ docker build --pull --network none --tag codingteam/icfpc-2020 --file Dockerfile .
```

**You have to run this every time you change Haskell source or one of the *.sh
scripts.**

`--pull` ensures that you're building on top of the latest image
provided by the organizers. `--network none` ensures that you haven't added any
dependencies that aren't in the base image. **If build fails due to missing
dependencies, submit a PR against
https://github.com/icfpcontest2020/dockerfiles/blob/master/dockerfiles/haskell/Dockerfile.base**.

Now run something that will fake organizer's server. A simple netcat listening
for TCP connections on port 8000 will do:

```console
# GNU netcat
$ nc -l -p 8000

# BSD netcat
$ nc -p 8000
```

(on Windows, `choco install netcat` will install GNU netcat)

Then actually run the application:

* on Linux:

  Find out your host's IP that Docker uses (Docker [doesn't support
  host.docker.internal DNS
  entry](https://github.com/docker/for-linux/issues/264) on Linux):

  ```
  $ ip a show dev docker0
  4: docker0: <NO-CARRIER,BROADCAST,MULTICAST,UP> mtu 1500 qdisc noqueue state DOWN group default
  link/ether 02:42:19:fe:9f:cc brd ff:ff:ff:ff:ff:ff
  inet 172.17.0.1/16 brd 172.17.255.255 scope global docker0
     valid_lft forever preferred_lft forever
  inet6 fe80::42:19ff:fefe:9fcc/64 scope link
     valid_lft forever preferred_lft forever
  ```

  Run the app:

  ```
  $ docker run --rm codingteam/icfpc-2020 http://172.17.0.1:8000 playerKey
  ```

* on macOS and Windows:

  ```
  $ docker run --rm codingteam/icfpc-2020 http://host.docker.internal:8000 playerKey
  ```

In netcat you should see an HTTP request:

```
GET /?playerKey=whatever HTTP/1.1
Host: 172.17.0.1:8000
Accept-Encoding: gzip
```

If you type something in response and press Enter, the application will exit.

## Running locally

Start netcat in the same manner as described above, and then:

```console
$ stack run main -- http://host.docker.internal:8000 playerKey
```

## Running locally against the org's server

```console
$ stack run main -- --local
```

## Running parser executable

```console
$ stack run parser data/galaxy.txt
```

## Running evaluator executable

```console
$ stack run evaluator 123 data/simple.txt
```

## Running the `galaxy` program inside the evaluator

```console
$ stack run evaluator -- galaxy data/galaxy.txt 0 0
```

## Running the GUI

```console
$ cd IcfpcMmxx.Gui
$  dotnet run --project IcfpcMmxx.Gui
```
