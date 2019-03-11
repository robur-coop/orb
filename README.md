# orb: check opam package reproductibility

This tool can check if an opam package build is reproductible (cf.
https://reproducible-builds.org). It installs the package twice (different path
& time) and check that installed files have the same hash.

With option `--diffoscope`, mismatching files are copied and theyr diff using
[`diffoscope`](https://diffoscope.org/) generated.

This project is currently in early beta.

## How does it work?

`orb` uses an already installed opam & opam root.
1 - Install of two new switches in `/tmp`
2 - Install of packages dependencies
3 - Install of required packages
4 - Retrieves hashes of installed files and look for mismatches
5 - (opt) Generate diffoscope output on mismatching files
