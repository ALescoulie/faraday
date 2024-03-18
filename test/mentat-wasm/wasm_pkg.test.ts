const fs = require("fs");

async function loadWasmModule(): Promise<WebAssembly.Module> {
    // Fetch the WebAssembly file
    const response = fs.readFileSync(`${__dirname}/mentat-interop.wasm`);

    // Compile the WebAssembly module
    return WebAssembly.compile(response);
}


async function inspectExports(): Promise<string[]> {
    const wasmModule = await loadWasmModule();

    // Get the exports from the module
    const exports = WebAssembly.Module.exports(wasmModule);


    // List the exported functions
    return exports.map((exp: any) => exp.name);
}

describe("Testing that mentat-interop.wasm exports methods correctly", () => {
    test("Exports c_trans_mentat_program", () => {
        inspectExports().then(exportNames => {
            expect(exportNames.includes('c_trans_mentat_program'));
            expect(exportNames.includes('malloc'));
            expect(exportNames.includes('free'));
            expect(exportNames.includes('getString'));
            expect(exportNames.includes('getStringLen'));
            expect(exportNames.includes('freeStableCStringLen'));
        }).catch(error => {
            throw error;
        });
    })
});


