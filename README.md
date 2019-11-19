# orb: check opam package reproductibility

This tool can check if an opam package build is reproductible (cf.
https://reproducible-builds.org). It installs the package twice (different path
& time) and check that installed files have the same hash.

## Install & use

```
$ opam pin git+https://github.com/rjbou/orb
$ orb pkg [--diiffoscope]
```

This project is currently in early beta.

## How does it work?

`orb` uses an already installed `opam` & opam root, and it follows those steps:

- Install of two new switches in `/tmp`
- Install of packages dependencies
- Install of required packages
- Retrieves hashes of installed files and look for mismatches
- Remove temporary switches

With option `--diffoscope`, mismatching files are copied locally and their diff 
generated, using [`diffoscope`](https://diffoscope.org/).

As `orb` generates temporary switches, packages dependencies are installed each 
time (also compiler), which can be time consuming when working on a package. 
Option `--use-switches sw1,sw2` can be used to give reusable switches.

`--keep-switches`  option permit to keep those generated switches for 
investigation needs. To manually remove them, don't just remove directory, but 
use `opam switch remove <sw>`.
