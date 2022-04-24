[![Maven Release](https://badgen.net/maven/v/metadata-url/https/repo1.maven.org/maven2/org/polystat/odin/odin_2.13/maven-metadata.xml)](https://oss.sonatype.org/content/repositories/releases/org/polystat/odin/odin_2.13/ )

Odin (object dependency inspector) — a static analyzer for [EO programming language](https://github.com/cqfn/eo).

# ⚠️🚧 Work in progress 🚧⚠️

> The project is still in active development stage and might not be usable or fully documented yet.

# Documentation
General documentation is available [here](docs/general_information.md).

Documentation for the mutual recursion analyzer is available [here](docs/analysis/mutual_recursion_analyzer.md).

Documentation for the unjustified assumption analyzer is available [here](docs/analysis/unjustified_assumption_analyzer.md).

# Running

To run the project you will need [sbt](https://www.scala-sbt.org/1.x/docs/Setup.html) and JDK 8+.

## Sandbox

As of now there is no app to run and interact with, but one can play with the source code of the analyzer in the sandbox project.

The entrypoint is in `sandbox/src/main/scala/eo/sandbox/Sandbox.scala`.

The sandbox can be run via:

```shell
sbt sandbox/run
```

## Scala REPL

The source code can be ran in scala REPL via:

```shell
sbt console
```

This might be helpful for development.

# Project structure

## `core` project

Defines EO AST. All other projects should depend on this one.

## `backends` project

Defines backends for the static analyzer. A backend is something that produces an output from AST.

### `eolang` subproject

Backend that is able to generate EO program (now, roughly, represented as a list of strings) from EO AST.

## `utils` project

Convenient tools that are used by other parts of the analyzer. This project must not depend on any other project.

## `sandbox` project

Enables to interactively run and test source code of the analyzer. Facilitates the development and debug of the source code and enables to see intermediate results of the development. Any other project must not depend on it. 

For more details on the project organization see:

- `build.sbt` - main build configuration file
- `project/Dependendencies.scala` - lists dependencies and versions for the projects
- `project/Compiler.scala` - lists compiler flags used to compile the project and compiler plugins

# Development

`master` branch has the latest changes and must always be buildable. All the code changes are done through sending pull requests to the repositories. After [CI](#ci) has successfully finished and a reviewer has approved changes, the code can be merged to the `master` branch. 

## CI

When a pull request is sent to `master` or a branch with a pull request to `master` is pushed, the following checks will run via GitHub Actions:

- Build — all projects in the repository are built to check that the code compiles
- Test — all tests are ran to check that new changes have not broken the existing functionality
- Lint — run scalafix. If it fails run `sbt scalafixAll` and fix issues that are not autofixable manually.

  [scalafix official documentation](https://scalacenter.github.io/scalafix/docs/users/installation.html#sbt) tells that SemanticDB compiler plugin with `-Yrangepos` flag adds overhead to the compilation, so it is recommended to create a local `.jvmopts` file with the following content:
  ```
    -Xss8m
    -Xms1G
    -Xmx8G
  ```
- Check code formatting — the code formatting will be checked via `scalafmt`. If it fails run `sbt scalafmtAll` and commit the updated formatting.

For more information, see `.github/workflows/ci.yml`.
