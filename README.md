# quantum-interpreter

quantum-interpreter is a quantum computer simulator/quil interpreter. It is
written in haskell using parsec for parsing.

## Building
To build, run `stack build` and to install run `stack install`.

## Usage
To run a file called "in.quil" with 2 qubits, run `quantum-interpreter -i in.quil -q 2`

Help can be found with `quantum-interpreter --help`
