# Ruse

Lisp dialect heavily inspired by Scheme

## Installing

### Warning

You need to have my C library, pigeon, installed.

```bash
make install
```

This will compile and put the ruse executable in /usr/local/bin
This will also put the std ruse files into /usr/local/lib/ruse/

## Examples

For some examples, look through the examples directory

## The Future

As of right now, Ruse has no garage collector because everything is alloacted
in the arena allocator and freed when the program ends.

There are plans to ditch the arena and write a garbage collector, but not right 
now.
