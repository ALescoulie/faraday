import commonjs from "@rollup/plugin-commonjs";
import nodeResolve from "@rollup/plugin-node-resolve";
import typescript from "@rollup/plugin-typescript";

export default {
  input: "src/index.ts",
  output: {
    file: "build/index.js",
    format: "iife",
    sourcemap: true,
  },
  plugins: [
    nodeResolve({
      jsnext: true,
      browser: true,
    }),
    typescript(),
    commonjs({
      transformMixedEsModules: true,
    }),
  ],
};
