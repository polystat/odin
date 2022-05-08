[![Maven Release](https://badgen.net/maven/v/metadata-url/https/repo1.maven.org/maven2/org/polystat/odin/odin_2.13/maven-metadata.xml)](https://oss.sonatype.org/content/repositories/releases/org/polystat/odin/odin_2.13/ )

Odin (object dependency inspector) — a static analyzer for [EO programming language](https://github.com/cqfn/eo).

It is a part of [polystat](https://github.com/polystat/polystat) and [polystat-cli](https://github.com/nikololiahim/polystat-cli).  

# Running

To run the project one will need 
[sbt](https://www.scala-sbt.org/1.x/docs/Setup.html)
and JDK 8+.

## Sandbox

One can play with the analyzer in the sandbox project 
by modifying the present code snippets or adding new ones.  

File containing the entrypoint can be found [here](sandbox/src/main/scala/org/polystat/odin/sandbox/Sandbox.scala).

The sandbox can be run via:

```shell
sbt sandbox/run
```

## Scala REPL

The source code can be run in scala REPL via:

```shell
sbt console
```

This might be helpful for development.

# Project structure

## `core` project

Contains the EO AST. 

All other projects should depend on this one.

## `analysis` project

Contains the implementations of the analyzers
odin can use.


## `backends` project

Contains backends for the static analyzer. 
Backend is something that produces representations the EO AST.

### `eolang` backend

Backend that can generate EO programs from an AST.

## `utils` project

Convenient tools that are used by other parts of the analyzer. 

This project must not depend on any other project.

## `sandbox` project

Allows one to interactively run and manually test the analyzer. 
Facilitates the development and debug of the source code 
and allows one to see intermediate results of the development. 

Any other project must not depend on it. 

For more details on the project organization see:

- [build.sbt](build.sbt) - main build configuration file
- [project/plugins.sbt](project/plugins.sbt) -
lists sbt plugins
- [project/Dependendencies.scala](project/Dependendencies.scala) -
lists dependencies and versions for the projects
- [project/Compiler.scala](project/Compiler.scala) -
lists compiler flags and compiler plugins

# Development

`master` branch has the latest changes and must 
always be buildable. 
All the changes are to be done by creating a pull request.
Once the [CI](#ci) run has successfully 
finished and a reviewer has approved changes, 
the code can be manually merged to the `master` branch. 

## CI

When a pull request is sent to `master` or a branch
with a pull request to `master` is pushed, 
the following checks will run via GitHub Actions:

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
