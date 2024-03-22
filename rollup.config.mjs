import commonjs from "@rollup/plugin-commonjs";
import nodeResolve from "@rollup/plugin-node-resolve";
import resolve from "@rollup/plugin-node-resolve"
import typescript from "@rollup/plugin-typescript";
import babel from "@rollup/plugin-babel";
import replace from "@rollup/plugin-replace";
import json from "@rollup/plugin-json";

export default {
  input: "src/app/index.tsx",
  output: {
    file: "build/bundle.js",
    format: "iife"
  },
  plugins: [
    resolve({
      copyTo: 'dist/libs',
      desDir: './libs'
    }),
    nodeResolve({
      extensions: ['.ts', '.tsx', 'js', 'jsx'],
      browser: true
    }),
    typescript(),
    json(),
    babel({
      babelHelpers: "bundled",
      presets: ['@babel/preset-react'],
      extensions: ['.ts', '.tsx', 'js', 'jsx'],
    }),
    commonjs(),
    replace({
      preventAssignment: false,
      transformMixedEsModules: true,
      'process.env.NODE_ENV': '"development"'
    })
  ],
};

