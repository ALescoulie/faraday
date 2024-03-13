async function loadMentatInterface(): Promise<WebAssembly.Module> {
    const response = await fetch("wasm/mentat-interop.wasm");
    const buffer = await response.arrayBuffer();

    return WebAssembly.compile(buffer);
}

async function compileMentat(mentatLines: [string]): Promise<string> {
    const mentatWasm = await loadMentatInterface();

    const instance = await WebAssembly.instantiate(mentatWasm);

    const translateMentatProgram = instance.exports.c_trans_mentat_program as (mentatLines: [string]) => string;

    return translateMentatProgram(mentatLines);
}

