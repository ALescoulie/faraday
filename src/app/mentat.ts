import { initMentatInterop, MentatInterop } from './mentat-interop'

interface MentatInput {
    pgLines: string[];
    domVars: string[];
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();

export class MentatCompiler {
    #mentat: MentatInterop

    constructor(mentat: MentatInterop) {
        this.#mentat = mentat;
    }

    compileMentatJson(mentatLines: string[], domainVars: string[]): String {

        console.log("putting program onto the stack");
        
        const mentatIn: MentatInput = {
            pgLines: mentatLines,
            domVars: domainVars
        };

        const inputJson = JSON.stringify(mentatIn);
        var output = this.#mentat.translateMentatProgram(inputJson);
        
        return output;
    }
}


export async function initMentatCompiler(wasmBinaryPath: string): Promise<MentatCompiler> {
    const  mentat = await initMentatInterop("wasm/mentat-interop.wasm");
    return new MentatCompiler(mentat);
}


enum CompOp {
    Eql = "Eql",
    Geq = "Geq",
    Leq = "Leq",
    G = "G",
    L = "L"
}

type Literal = number | boolean;

type MentatVariables = Map<string, Literal>

type MentatFunction = (vars: MentatVariables, funcs: Map<String, MentatFunction>, ...args: Literal[]) => Literal;

type MentatFunctions = Map<string, MentatFunction>


interface MentatConstraint {
    left: (vars: MentatVariables, funcs: MentatFunctions, ...args: Number[]) => number
    right: (vars: MentatVariables, funcs: MentatFunctions, ...args: Number[]) => number
    comparison: CompOp
}


interface MentatProgram {
    vars: MentatVariables
    funcs: MentatFunctions
    expressions: Literal[]
    constraints: MentatConstraint[]
}

