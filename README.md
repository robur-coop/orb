# orb: check opam package reproducibility

This tool can check if an opam package build is reproductible (cf.
https://reproducible-builds.org). It has two subcommands: build and rebuild.

The orb build conducts a build of an opam package, and collects the build result
(and hashes thereof) and the build input (or build info), consisting of:
- build-environment (the environment variables plus OS/OS_DISTRIBUTION/OS_FAMILY/OS_VERSION)
- system-packages (the installed packages on the system)
- opam-switch (opam switch export --full --freeze - a textual representation containing all installed opam packages)
- *.build-hashes with maps of installed files to their hashes

The orb rebuild takes this data as input and conducts a second build with the same environment, and compares that the hashes of the produced binaries are identical.

The orb build also has a command-line flag "--twice" to conduct a build and a
rebuild directly afterwards. For debugging reproducibility, a "--keep-build"
(useful with opam's "--keep-build-dir") option is provided that allows to
compare intermediate build products as well.

Please have a look at "--diffoscope", "--out", "--switch-name",
"--solver-timeout", "--date", and other command line parameters.

It is currently used as a payload of
[builder-worker](https://github.com/roburio/builder) to run the
[reproducible MirageOS unikernels](https://builds.robur.coop) infrastructure.

Binary packages for different platforms (Debian, Ubuntu, FreeBSD) are available
at https://builds.robur.coop

## Install & use

```
$ opam pin git+https://github.com/roburio/orb#next
$ orb build --twice --repos=default:https://opam.ocaml.org cmdliner
```

Simple (and fast) failing and successful reproducible opam packages are in the
[reproducible-testing-repo](https://github.com/roburio/reproducible-testing-repo).
