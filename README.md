Static analyzer for [EO programming language](https://github.com/cqfn/eo).

# ⚠️🚧 Work in progress 🚧⚠️ 

> The project is still in active development stage and might not be usable or fully documented yet.

# Running

To run the project you will need [sbt](https://www.scala-sbt.org/1.x/docs/Setup.html) and JDK 8+.

## Sandbox

As of now there is no app to run and interact with, but one can play with the source code of the analyzer in the sandbox project.

The entrypoint is in `snadbox/src/main/scala/eo/sandbox/Sandbox.scala`.

The sandbox can be run via:

```shell
sbt sandbox/run
```

> Currently, the sandbox contains a program, that represents AST of mutually recursive EO program, transforms the AST to EO and prints it.

# Project structure

For more details on the project organization see:

- `build.sbt` - main build configuration file
- `project/Dependendencies.scala` - lists dependencies and versions for the projects
- `project/Compiler.scala` - lists compiler flags used to compile the project and compiler plugins
