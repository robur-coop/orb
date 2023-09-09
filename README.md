# orb: check opam package reproducibility

This tool can check if an opam package build is reproductible (cf.
https://reproducible-builds.org). It has two subcommands: build and rebuild.

The orb build conducts a build of an opam package, and collects the build result
(and hashes thereof) and the build input (or build info), consisting of:
- build-environment (the environment variables plus OS/OS_DISTRIBUTION/OS_FAMILY/OS_VERSION/ORB_VERSION)
- system-packages (the installed packages on the system)
- opam-switch (opam switch export --full --freeze - a textual representation containing all installed opam packages)
- *.build-hashes with maps of installed files to their hashes

The orb rebuild takes this data as input and conducts a second build with the same environment, and compares that the hashes of the produced binaries are identical.

The orb build also has a command-line flag "--twice" to conduct a build and a
rebuild directly afterwards. For debugging reproducibility, the "--keep-build-dir"
option is provided that allows to compare intermediate build products as well.

Please have a look at "--out", "--switch-name",
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

## History

The initial development was done by Raja Boujbel (working at OCamlPro) in spring 2019 at the MirageOS retreat in Marrakesh.
Another iteration was done in December 2019 by Hannes Mehnert at the reproducible builds summit.
In 2020 and 2021, Reynir Björnsson contributed together with Hannes Mehnert more adjustments to use orb on FreeBSD and Debian for conducting reproducible builds of MirageOS unikernels.
In 2022, the robur team contributed work to use orb with MirageOS 4, which uses opam-monorepo.
