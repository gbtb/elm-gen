/// <reference path="elm.d.ts" />
import * as parseArgs from 'minimist'
import * as fs from 'fs';
import * as path from 'path';
import { parseGenCommand } from './Utils';

const usageStr = "usage: elm-gen [input_file] [output_file]";
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
    const inputFileName = path.basename(inputPath).split('.')[0];
    const outFileName = `${inputFileName}Decoders.elm`;

    outPath = path.join(outPath, outFileName);

    const Elm =  require("../elm/src/Main.elm");
    const app = Elm.Main.worker();

    app.ports.output.subscribe((string: string) => {
        console.log(`Writing output to file: ${outPath}`);
        fs.writeFileSync(outPath, string);
    });

    app.ports.logMessage.subscribe((msg: string) => {
        console.log(msg);
    });

    app.ports.errorMessage.subscribe((msg: string) => {
        console.error(msg);
        process.exit(1);
    });

    const fileContent: string = fs.readFileSync(inputPath, {encoding: 'utf-8'});
    app.ports.input.send([fileContent, [inputPath] ]);

    //port for requesting of additional files to get type dependencies
    app.ports.requestFiles.subscribe((files: [[string]]) => {
        const {fileContents, error} = getFileContents(inputPath, files);
        if (error != null){
            console.error(error);
            process.exit(1);
        }

        app.ports.input.send([fileContents, files.map(l => l.join(".") + ".elm"), genCommand]);
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
        return {fileContents: null, error: e}
    }
}