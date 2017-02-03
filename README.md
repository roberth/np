
# The Complete *N*ix *P*roject Tool

[Nix](https://nixos.org/nix) is a great tool for gaining absolute control over software dependencies and deployment. However, it is not, nor should it be, a do-it-all ‘project management tool’ like some of the language-specific package managers have become.

`np` stands for **n**ix **p**roject and its purpose is to provide easy access to the various commands that a developer may want to use for project automation.

`np` provides the advantages:

 - No need to install language-specific tools like `cabal2nix`, `npm2nix` etc.
 - The version controlled project configuration is in charge of the versions of its tools
 - A general interface for all languages and project types

# Install

`nix-env -iA nixpkgs.haskellPackages.np` or `nix-env -i -f .` in this directory.

# Example use

```
$ cd my-project
$ np help

my-project supports the following commands:

  - build
  - test
  - update-dependencies
  - release

$ np update-dependencies
```

# How it works

The `my-project` directory contains a `nix-project.yaml` file that contains at least an attribute `nix-project-tool`. The value of this attribute points the `np` tool to the right Nix expression. An example:

```yaml
nix-project-tool:
  location: some-relative-path
```

An optional attribute may be provided, and the location can also be a git commit. For example:

```yaml
nix-project-tool:
  attribute: nix-project-foo-bar
  location:
    git: https://github.com/me/smallrepo.git
    rev: 95a6f9fda955f98ad985a598fd8959a59f598affa
    sha256: 1y62n4h70kh9a0pyigssgwfrdc1kfx9i4743722f2c6kf67j1v20
```

The git commit will be retrieved using the `fetchgit` function in the system `<nixpkgs>`.

Alternatively, any Nix expression can be used:

```
nix-project-tool:
  # This <nixpkgs> makes the build harder to reproduce though!
  expression: import <nixpkgs> {}
```

# Standard commands

All commands must
 - use the exit status to indicate success or failure
 - accept a `--help` flag that explains the command in full detail
 - prefer sanity over efficiency by default. Example:
     - incremental build is a sane default
     - incremental testing is **not** a sane default
     - incremental mutation testing is a sane default

Please make suggestions and pull requests for these commands.

## `help`

Documents the other commands.

## `build`

Perform the minimal amount of work to produce the final artifacts.

## `qa`

Perform all steps that are designed to aid product quality.

`--skip-integration`: do not perform tests that are too slow or may be impossible to run on the developer's system.

## `watch` <command>

Detect file changes and perform the command, or similar steps, as appropriate.

## clean

Purge all project-specific caches.

# Writing `np` tools

The derivation should produce an executable at `$out/bin/.nix-project-tool`. It should support at least the `help` command.

`np` will only read `nix-project-tool` from the `nix-project.yaml` file, ignoring the rest. Feel free to put project settings in it.

`np` will `cd` to the directory of `nix-project.yaml` for consistency. If for some command, you want the behavior to be specific to the directory where it was invoked, you can recover that information through the `NIXPROJECT_FOCUS` environment variable.

`np` will choose the `nix-project.yaml` file that is highest up in the directory hierarchy, unless specified with `-f`. It may make sense to have special support for such modular projects where multiple relevant `nix-project.yaml` files exist.

# Areas of improvement

`np` should not need to change much due to its small size and by virtue of being a naturally extensible tool.

That leaves the following:

  - self-host with a simple np-cabal integration
  - nix-project-tools for other languages
     - add them to nixpkgs
  - Support for bash completion (does optparse-applicative support the passthrough?)
  - A generic nix-project-tool for recursive projects (if that makes sense)
