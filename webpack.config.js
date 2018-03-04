const path = require('path');
const CleanWebpackPlugin = require('clean-webpack-plugin');
const BannerPlugin = require('banner-webpack-plugin');
const WebpackShellPlugin = require('webpack-shell-plugin');

function getMakePath()
{
  if (process.env.TRAVIS){

  }
}

module.exports = {
  target: 'node',
  entry: {  
    "elm-gen":   './ts/Main.ts',
    tests:  './ts/MainTests.ts',
    "unit-tests": './ts/UnitTests.ts'
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {
            cwd: './elm/src'
          }
        }
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
