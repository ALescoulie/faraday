import { MentatCompiler, initMentatCompiler } from '../../src/app/mentat.ts'



describe ("Testing mentat run", () => {
    test("translates program correctly", async () => {
        const compiler = await initMentatCompiler(`${__dirname}/mentat-interop.wasm`)
        const mentatLines = ["b := 1", "m := 2", "y = mx + b", "f(x) := 2x", "f(2)^3"];
        const domVars = ["x", "y"];
        expect("" == compiler.compileMentatJson(mentatLines, domVars));
    })
});

