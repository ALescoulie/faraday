export namespace Mentat {

    export interface MentatInput {
        pgLines: string[];
        domVars: string[];
    }


    export enum CompOp {
        Eql = "Eql",
        Geq = "Geq",
        Leq = "Leq",
        G = "G",
        L = "L"
    }

    export type Literal = number | boolean;

    export type MentatVariables = Map<string, Literal>

    export type MentatFunction = (vars: MentatVariables, funcs: Map<String, MentatFunction>, ...args: Literal[]) => Literal;

    export type MentatFunctions = Map<string, MentatFunction>


    export interface MentatConstraint {
        left: (vars: MentatVariables, funcs: MentatFunctions, ...args: Number[]) => number
        right: (vars: MentatVariables, funcs: MentatFunctions, ...args: Number[]) => number
        comparison: CompOp
    }


    export interface MentatProgram {
        vars: MentatVariables
        funcs: MentatFunctions
        expressions: Literal[]
        constraints: MentatConstraint[]
    }
}

