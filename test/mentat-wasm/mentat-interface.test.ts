import { MentatCompiler } from '../../src/app/mentat.ts'
import fetch, { Response } from 'node-fetch'
import { readFile } from 'fs/promises'


async function getWasmFile(): Promise<WebAssembly.Instance> {
    const file = await readFile(`${__dirname}/mentat-interop.wasm`);
    const inst = await WebAssembly.instantiate(file, { imports: {} });
    return inst.instance 
}

describe ("Testing mentat run", () => {
    test("translates program correctly", async () => {
        const wasm = await getWasmFile();
        const compiler = new MentatCompiler(wasm);
        const mentatLines = ["b := 1", "m := 2", "y = mx + b", "f(x) := 2x", "f(2)^3"];
        const domVars = ["x", "y"];
        expect("" == compiler.compileMentatJson(mentatLines, domVars));
    })
});

