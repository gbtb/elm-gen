/// <reference path="elm.d.ts" />
import * as parseArgs from 'minimist'
import * as fs from 'fs';
import * as path from 'path';

const usageStr = "usage: elm-gen [input_file] [output_file]";
const args = parseArgs(process.argv);

//first arg is node exec
//second arg is elm-gen file itself
if (!args._ ||  args._.length != 4){
    console.error("Error: Required arguments not provided!");
    console.log(usageStr);
    process.exit(1);
}else{
    const inputPath = args._[2];
    var outPath = args._[3];
    const inputFileName = path.basename(inputPath).split('.')[0];
    const outFileName = `${inputFileName}Decoders.elm`;

    outPath = path.join(outPath, outFileName);
    console.log(`Writing output to file: ${outPath}`);

    const Elm =  require("../elm/src/Main.elm");
    const app = Elm.Main.worker();
    app.ports.output.subscribe(function (string: String) {
        fs.writeFileSync(outPath, string);
    });

    const fileContent: string = fs.readFileSync(inputPath, {encoding: 'utf-8'});
    app.ports.input.send(fileContent);

    

}