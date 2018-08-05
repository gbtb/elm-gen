![](https://travis-ci.org/gbtb/elm-gen.svg?branch=master) [![npm version](https://badge.fury.io/js/elm-gen.svg)](https://badge.fury.io/js/elm-gen)
# elm-gen
Experimental CLI tool for generating Elm JSON Decoders and Encoders, written mostly in Elm. 
Based on top of [elm-ast](https://github.com/Bogdanp/elm-ast) parsing library.

## Usage

### Online demo 
    Simplified demonstration available [here](https://gbtb.github.io/elm-gen/).

### CLI tool
CLI tool can be obtained from [NPM](https://www.npmjs.com/package/elm-gen). After installation, you can used it like that:
`elm-gen decoders&encoders Model.elm .` Also note, that generated elm code for decoders uses NoRedInk elm-decode-pipeline package functions, so it must be installed into your project.
For more examples you may also look at functional tests in ts/MainTests.ts and correspondent files in tests_data folder.

##Features

* Generation of decoders and/or encoders for user defined record and union types without type variables.
* elm-gen supports basic elm types supported by Json.(Encode|Decode).


### Config
Some configuration (mostly about naming) can be applied from config json-file. Example of config can be found in tests_data folder.
It's correspondent Elm type representation resides in Config.elm

### Meta-Comments
Elm-gen supports meta-comments in source code, in form of either `-- //Meta-Comment` or `{-| //Meta-Comment -}`.  
For now, there are 3 meta-comments:

* Ignore -- elm-gen completely ignores next type definition (literally skipping it during parse stage).

* DefaultValue -- use next pair of (value type def, value expr) to produce default value for `Json.Decode.Pipeline.optional` function.
Supports values of types `defaultValue: UnionType` for union types, and `defaultValue : Record ` or `defaultValue : Record -> Record` for record types.

* NoDeclaration -- elm-gen won't generates type declaration for next type's decoder (usefull for preserving record structural typing ability for generated decoder )
