![](https://travis-ci.org/gbtb/elm-gen.svg?branch=master) [![npm version](https://badge.fury.io/js/elm-gen.svg)](https://badge.fury.io/js/elm-gen)
# elm-gen
Experimental CLI tool for generating Elm JSON Decoders and Encoders, written mostly in Elm. 
Based on top of [elm-ast](https://github.com/Bogdanp/elm-ast) parsing library.

## Usage

### Online demo 
Simplified demonstration available [here](https://gbtb.github.io/elm-gen/).

### CLI tool
CLI tool can be obtained from [NPM](https://www.npmjs.com/package/elm-gen). After installation, you can used it like that:  
`elm-gen decoders&encoders Model.elm .`  
Call above will produce new file ModelEncodersAndDecoders.elm, which will contain decoders and encoders for types present in Model.elm.  
Also note, that generated elm code for decoders uses NoRedInk elm-decode-pipeline package functions, so it must be installed into your project.  
For more examples you may also look at functional tests in ts/MainTests.ts and correspondent files in tests_data folder.
## Features
* elm-gen can generate decoders and/or encoders for user defined Record and Union types without type variables.
* elm-gen supports basic elm types supported by Json.(Encode|Decode) plus tuples and Maybe's.
* elm-gen can use hand-made decoders and encoders `Decoder X` and `X -> Value` in order to generate decoders for types, dependent on type `X`
* elm-gen will follow **explicit** import type references to look for type definitions doesnot present in current file
* elm-gen dont require to break your Elm code in any way (its still compilable by `elm-make` without pre-processing), except for some [meta-comments](#meta-comments) insertion.

### Config
Some configuration (mostly about naming) can be applied from config json-file. Example of config can be found in tests_data folder.
It's correspondent Elm type representation resides in Config.elm

### Meta-Comments
Elm-gen supports meta-comments in source code, in form of either `-- //Meta-Comment` or `{-| //Meta-Comment -}`.  
For now, there are 4 meta-comments:

* Ignore -- elm-gen completely ignores next type definition (literally skipping it during parse stage).

* DefaultValue -- use next pair of (value type def, value expr) to produce default value for `Json.Decode.Pipeline.optional` function.
Supports values of types `defaultValue: UnionType` for union types, and `defaultValue : Record ` or `defaultValue : Record -> Record` for record types.

* NoDeclaration -- elm-gen won't generates type declaration for next type's decoder (usefull for preserving record structural typing ability for generated decoder )

* Field name aliases - You can define aliases for some field names to avoid issues with elm reserved words, and to make some *special* field names (like `_id` and `_rev` which stored by Couchdb) normally looking in Elm representation. For example, I use meta-comment and record to define some common aliases for Couchdb-mapped types: 
```
{-| //FieldNameMapping
-}
couchAliases =
    { id = "_id"
    , rev = "_rev"
    , type_ = "_type"
    }
```
And later I apply such mapping to other type via another meta-comment:
```
{-| //UseFieldNameMapping(couchAliases)
-}
type alias NewUser =
    { id : String
    , name : String
    , password : String
    , type_ : String
    , ...
    }
```

