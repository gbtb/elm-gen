import * as mocha from 'mocha';
import * as chai from 'chai';
import * as shell from 'shelljs';
import * as fs from 'fs';
import * as path from 'path';

const expect  = chai.expect;
const execDir = './';
const dataDir = '../tests_data/';

function readFile(filePath: string){
  return fs.readFileSync(filePath, { encoding: 'utf8' });
}

function outPath(filename: string){
  return path.join(execDir, filename);
}

function dataPath(filename: string){
  return path.join(dataDir, filename);
}

describe('Elm-gen by default produces decoder and encoders', () => {
  it('yells if no arguments provided' , () => {
    const ret = shell.exec("./dist/elm-gen");
    expect(ret.stderr).to.equal(
      "Error: Required arguments not provided!\n"
    );
    expect(ret.stdout).to.contain(
      "usage: "
    );
  });  

  it('for basic type alias' , () => {
    shell.cd('./dist');
    const ret = shell.exec("./elm-gen d ../tests_data/Basic.elm .");

    expect(
      readFile(outPath("BasicDecoders.elm"))
    ).to.equal(
      readFile(dataPath("BasicDecoders.elm"))
    );

    expect(ret.stdout).to.contain("Parsing files...");
    expect(ret.stdout).to.contain("Parsing is complete, all required types are loaded...");
    expect(ret.stdout).to.contain("Generating decoders...");
    expect(ret.stdout).to.contain("Printing...");
  });

  

  it('generates decoder&encoders for tuples' , () => {
    shell.exec("./elm-gen d,e ../tests_data/Tuples.elm .");
    
    expect(
      readFile(outPath("TuplesDecoders.elm"))
    ).to.equal(
      readFile(dataPath("TuplesDecoders.elm"))
    );
  });

});