const path = require('path');
const CleanWebpackPlugin = require('clean-webpack-plugin');
const BannerPlugin = require('banner-webpack-plugin');
const WebpackShellPlugin = require('webpack-shell-plugin');

module.exports = {
  target: 'node',
  entry: {  
    "elm-gen":   './ts/Main.ts',
    tests:  './ts/MainTests.ts',
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/
      }
    ]
  },
  resolve: {
    extensions: [ '.ts', '.js' ]
  },
  output: {
    filename: '[name]',
    path: path.resolve(__dirname, 'dist')
  },
  plugins: [
    new CleanWebpackPlugin(['dist']),
    new BannerPlugin({
      chunks : {
        "elm-gen": {
          beforeContent: '#!/usr/bin/env node\n'
        }
      }
    }),
    new WebpackShellPlugin({
      onBuildEnd:['chmod +x dist/elm-gen'],
    }),
  ]
};
