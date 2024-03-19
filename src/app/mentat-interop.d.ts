declare class MentatInterop {

    constructor(mentatInst: WebAssembly.instance);
    
    translateMentatProgram(mentatInputJson: string): string;
}

declare async function initMentatInterop(wasmPath: string): Promise<MentatInterop>;

export { MentatInterop, initMentatInterop };

