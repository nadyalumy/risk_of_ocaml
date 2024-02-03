# risk_of_ocaml

## Installation and Dependencies
This project depends on the [OCaml bindings for Raylib](https://github.com/tjammer/raylib-ocaml) ([Raylib](https://www.raylib.com/)), the [OCaml bindings for Raygui](https://opam.ocaml.org/packages/raygui/raygui.0.6.0/) ([Raygui](https://github.com/raysan5/raygui)), and [Yojson](https://github.com/ocaml-community/yojson) Use the package manager [opam](https://opam.ocaml.org/) to install these dependencies.

In a terminal instance, run
```bash
opam depext raylib
```
This command will install the C dependencies of Raylib. Now run
```bash
opam install raylib
```
to install the Raylib library for OCaml. Then run
```bash
opam install raygui
```
to install Raygui for OCaml. Lastly, run
```bash
opam install yojson
```
to install Yojson.

## Running the game
In the source directory of the project, run
```bash
dune build
```
to build the project. Then run
```bash
make play
```
to run [risk_of_ocaml](https://github.coecis.cornell.edu/jp2369/3110proj) in terminal.