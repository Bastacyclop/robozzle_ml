type symbol = string

type instruction =
    | Move
    | Rotate of Puzzle.rotation
    | Color of Puzzle.color
    | If of Puzzle.color * instruction
    | Call of symbol

type definition = Definition of symbol * instruction list

type program = Program of definition list

val compile: program -> Vm.bytecode
