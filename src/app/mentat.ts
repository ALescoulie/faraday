import { WASI } from '@bjorn3/browser_wasi_shim'

interface MentatInput {
    pgLines: string[];
    domVars: string[];
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();

export class MentatCompiler {
    #hsMalloc: CallableFunction
    #hsFree: CallableFunction
    #getHsStrLen: CallableFunction
    #getHsStr: CallableFunction
    #freeHsStrLen: CallableFunction
    #transMentatPg: CallableFunction
    #hsMem: WebAssembly.Memory

    constructor(mentatWasm: WebAssembly.Instance) {
        this.#hsFree = mentatWasm.exports.free as CallableFunction;
        this.#hsMalloc = mentatWasm.exports.malloc as CallableFunction;
        this.#freeHsStrLen = mentatWasm.exports.freeStableCStringLen as CallableFunction;
        this.#getHsStr = mentatWasm.exports.getString as CallableFunction;
        this.#getHsStrLen = mentatWasm.exports.getStringLen as CallableFunction;
        this.#transMentatPg = mentatWasm.exports.c_trans_mentat_program as CallableFunction;
        this.#hsMem = mentatWasm.exports.memory as WebAssembly.Memory;
    }

    compileMentatJson(mentatLines: string[], domainVars: string[]): String {

        console.log("putting program onto the stack");
        
        const mentatIn: MentatInput = {
            pgLines: mentatLines,
            domVars: domainVars
        };

        const inputJson = JSON.stringify(mentatIn) + '\0';

        const inJsonBytes = encoder.encode(inputJson);

        const inLen = inJsonBytes.byteLength;

        console.log("Allocating memory for input");
        const inPtr = this.#hsMalloc(inLen);
        
        console.log("writing input to memory buffer");
        new Uint8Array(this.#hsMem.buffer, inPtr, inLen).set(inJsonBytes);

        console.log("translating mentat program")
        const outCStrLen = this.#transMentatPg(inPtr, inLen);
        try {
            const outJsonLen = this.#getHsStrLen(outCStrLen);
            const outJsonPtr = this.#getHsStr(outCStrLen);
            const outJsonBytes = new Uint8Array(this.#hsMem.buffer, outJsonPtr, outJsonLen);
            var output = decoder.decode(outJsonBytes);
        } finally {
            console.log("freeing memory");
            this.#freeHsStrLen(outCStrLen);
            this.#hsFree(inPtr);
        }
        return output;
    }
}


export async function initMentatCompiler(wasmBinaryPath: Response): Promise<MentatCompiler> {
    const wasm = await WebAssembly.instantiateStreaming(wasmBinaryPath);
    return new MentatCompiler(wasm.instance);
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

