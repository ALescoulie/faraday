import commonjs from "@rollup/plugin-commonjs";
import nodeResolve from "@rollup/plugin-node-resolve";
import typescript from "@rollup/plugin-typescript";
import babel from "@rollup/plugin-babel";
import replace from "@rollup/plugin-replace";

export default {
  input: "src/app/index.tsx",
  output: {
    file: "build/bundle.js",
    format: "iife",
  },
  plugins: [
    nodeResolve({
      extensions: ['.ts', '.tsx', 'js', 'jsx']
    }),
    typescript(),
    babel({
      babelHelpers: "bundled",
      presets: ['@babel/preset-react'],
      extensions: ['.ts', '.tsx', 'js', 'jsx']
    }),
    commonjs(),
    replace({
      preventAssignment: false,
      'process.env.NODE_ENV': '"development"'
    })
  ],
};

