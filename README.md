[](https://travis-ci.org/gbtb/elm-gen.svg?branch=master)
# elm-gen
Experimental CLI tool for generating Elm JSON Decoders and Encoders, written mostly in Elm. 
Based on top of [elm-ast](https://github.com/Bogdanp/elm-ast) parsing library.

## Usage
`elm-gen decoders&encoders Model.elm .`  
Also look at functional tests in ts/MainTests.ts and correspondent files in tests_data folder.

### Meta-Comments
Elm-gen supports meta-comments in source code, in form of either `-- //Meta-Comment` or `{-| //Meta-Comment -}`.  
For now, there are 3 meta-comments:

* Ignore -- elm-gen completely ignores next type definition (literally skipping it during parse stage).

* DefaultValue -- use next pair of (value type def, value expr) to produce default value for `Json.Decode.Pipeline.optional` function.
Supports values of types `defaultValue: UnionType` for union types, and `defaultValue : Record ` or `defaultValue : Record -> Record` for record types.

* NoDeclaration -- elm-gen won't generates type declaration for next type's decoder (usefull for preserving record structural typing ability for generated decoder )
