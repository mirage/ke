Ke - Fast implementation of Queue in OCaml
==========================================

![travis-ci](https://travis-ci.org/mirage/ke.svg?banch=master)

Queue or FIFO is one of the most famous data-structure used in several
algorithms. `Ke` provides some implementations of it in a functional or
imperative way.

It is a little library with a benchmark
([`bechamel`](https://github.com/dinosaure/bechamel.git) or `core_bench`),
a fuzzer and tests.

We provide a functional interface `Fke` or an imperative interface `Rke`.

From what we know, `Ke.Rke` is faster than `Queue` from the
standard library or the `base` package. The type of data that it can store
is limited (only supports the types supported by [`Bigarray.kind`](https://v2.ocaml.org/releases/5.1/api/Bigarray.html#TYPEkind))
, but this is enough for a lot of algorithms. The fast
operation is to put some elements faster than a sequence of `Queue.push`, and
get some elements faster than a sequence of `Queue.pop`.

We extended implementations to have a limit of elements to store (see
`Rke.Weighted` and `Fke.Weighted`). The purpose of it is to limit memory
consumption of the queue when we use it in some contexts (like _encoder_).

Again, as a part of the MirageOS project, `Ke` does not rely on C stubs,
`Obj.magic` and so on.

Author: Romain Calascibetta <romain.calascibetta@gmail.com>

Documentation: https://mirage.github.io/ke/

Implementation notes
====================

The functional implementation `Fke` comes from Okazaki's queue
implementation with GADT to discard impossible cases.

`Rke`, `Rke.Weighted` and `Fke.Weighted` are limited by kind and follow Xen's
implementation of the shared memory ring-buffer. The length of the internal buffer
is always a power of two - that means for a large number of elements
this kind of queue may not fit your requirements.

A fuzzer was made to compare the standard `Queue` (as an oracle) with `Rke` and
`Fke`. We construct a set of actions (`push` and `pop`) and ensure (by GADT) to
never `pop` an empty queue.
