import { WASI } from '@bjorn3/browser_wasi_shim'

const encoder = new TextEncoder();
const decoder = new TextDecoder();

class MentatInterop {
    #hs

    constructor(mentatInst) {
        this.#hs = mentatInst.exports;
    }

    #free(ptr) {
        this.#hs.free(ptr);
    }

    #malloc(size) {
        return this.#hs.malloc(size);
    }

    #freeCStrLen(cStrLen) {
        this.#hs.freeStableCStringLen(cStrLen);
    }
    
    #getString(cStrLen) {
        return this.#hs.getString(cStrLen);
    }

    #getStringLen(cStrLen) {
        return this.#hs.getStringLen(cStrLen);
    }
    
    // Frees memory for caller
    translateMentatProgram(mentatInputJson) {
        const jsonBytes = encoder.encode(mentatInputJson + '\0');
        
        const inLen = jsonBytes.byteLength;

        console.log("allocating memory for input");

        const inPtr = this.#malloc(inLen);

        console.log("writing input to memory buffer");

        new Uint8Array(this.#hs.memory.buffer, inPtr, inLen).set(jsonBytes);

        console.log("translateMentatProgram");
        const outCStrLen = this.#hs.c_trans_mentat_program(inPtr, inLen);
        try {
            const outJsonLen = this.#getStringLen(outCStrLen);
            const outJsonPtr = this.#getString(outCStrLen);
            const outJsonBytes = new Uint8Array(this.#hs.memory.buffer, outJsonPtr, outJsonLen);
            var output = decoder.decode(outJsonBytes);
        } finally {
            console.log("freeing memory");

            this.#freeCStrLen(outCStrLen);
            this.#free(inPtr);
        }

        return output;
    }
}


export async function initMentatInterop(wasmPath) {
    const wasi = new WASI([], [], []);
    const wasm = await WebAssembly.instantiateStreaming(
        fetch(wasmPath),
        {"wasi_snapshot_preview": wasi.wasiImport}
    );

    wasi.inst = wasm.instance;
    return new MentatInterop(wasm.instance);
}

