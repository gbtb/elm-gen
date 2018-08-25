/// <reference path="elm.d.ts" />
import * as parseArgs from 'minimist'
import * as fs from 'fs';
import * as path from 'path';
import { parseGenCommand, getOutputFileName } from './Utils';

const version = "MY_VERSION";

const usageStr = `elm-gen ${version}
usage: elm-gen [command] [input_file] [output_file] options
Command can be: decoders | encoders | decoders,encoders and also shortcutted to d,e.
For now single input_file and output_file only supported.
Available options:
    --rootDir   [path]  path to directory, from which to count Elm's module hierarchy
    --config    [path]  path to config file. Defaults to elm-gen.json
`;
const args = parseArgs(process.argv);

//first arg is node exec
//second arg is elm-gen file itself
if (!args._ ||  args._.length < 4){
    console.error("Error: Required arguments not provided!");
    console.log(usageStr);
    process.exit(1);
}else{
    const genCommand = parseGenCommand(args._[2]);
    const inputPath = args._[3];
    var outPath = args._[4];
    const outFileName = path.basename(inputPath);//true file name calculated on elm's side

    outPath = path.resolve(
        path.join(outPath, outFileName)
    );

    const Elm =  require("../elm/src/Main.elm");
    const app = Elm.Main.worker();

    app.ports.output.subscribe((out: Array<string>) => {
        console.log(`Writing output to file: ${out[0]}`);
        fs.writeFileSync(out[0], out[1]);
    });

    app.ports.logMessage.subscribe((msg: string) => {
        console.log(msg);
    });

    app.ports.errorMessage.subscribe((msg: string) => {
        console.error(msg);
        process.exit(1);
    });

    let configPath = "./elm-gen.json";
    if (args['config'])
        configPath = args['config'];

    let rootDir = process.cwd();
    if (args['rootDir'])
        rootDir = args['rootDir'];

    rootDir = path.resolve(rootDir);
    configPath = path.resolve(configPath);

    if (fs.existsSync(configPath)){
        console.log(`Reading config: ${configPath}`)
        const configContents = fs.readFileSync(configPath, {encoding: 'utf-8'});
        app.ports.config.send(configContents);
    }

    const fileContents: string = fs.readFileSync(inputPath, {encoding: 'utf-8'});
    app.ports.input.send({
        fileContents: fileContents, 
        rootDir: rootDir,
        fileNames: [outPath],
        genCommand: genCommand 
    });

    //port for requesting of additional files to get type dependencies
    app.ports.requestFiles.subscribe((files: [[string]]) => {
        const {fileContents, error} = getFileContents(inputPath, files);
        if (error != null){
            console.error(error);
            process.exit(1);
        }

        app.ports.input.send({
            fileContents: fileContents, 
            rootDir: process.cwd(),
            fileNames: files.map(l => l.join(".") + ".elm"),
            genCommand: genCommand 
        });
    });

}

//TODO: proper handling of relative paths, add cwd param to cli
function getFileContents(inputPath: string, files: [[string]]){
    try {
        const dir = path.dirname(inputPath);
        const fileContents = files.map((fileName) => {
            return fs.readFileSync(
                path.join(dir, fileName.join("/") + ".elm"), 
                {encoding: 'utf-8'}
            )
        }).join("\n");
        return {fileContents: fileContents, error: null}
    }catch(e){

        return {fileContents: null, error: e.message}
    }
}