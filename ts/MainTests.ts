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

describe('My test tools', () => {

  it('just working' , () => {
    expect(3).to.equal(3);
  });

});
describe('Elm-gen by default produces decoder and encoders', () => {
  it('yells if no arguments provided' , () => {
    const ret = shell.exec("./dist/elm-gen");
    expect(ret.stderr).to.equal(
      "Error: Required arguments not provided!\n"
    );
    expect(ret.stdout).to.equal(
      "usage: elm-gen [input_file] [output_file]\n"
    );
  });  

  it('for basic type alias' , () => {
    shell.cd('dist');
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

  it('for simple dependent types' , () => {
    shell.exec("./elm-gen d ../tests_data/DependentTypes.elm .");
    expect(
      readFile(outPath("DependentTypesDecoders.elm"))
    ).to.equal(
      readFile(dataPath("DependentTypesDecoders.elm"))
    );
  });

  it('for types splitted into two files' , () => {
    shell.exec("./elm-gen d ../tests_data/DependentOnOtherFile.elm .");
    expect(
      readFile(outPath("DependentOnOtherFileDecoders.elm"))
    ).to.equal(
      readFile(dataPath("DependentOnOtherFileDecoders.elm"))
    );
  });

  it('for three-levels deep type structure' , () => {
    shell.exec("./elm-gen d ../tests_data/ThreeLevelsDepth.elm .");
    expect(
      readFile(outPath("ThreeLevelsDepthDecoders.elm"))
    ).to.equal(
      readFile(dataPath("ThreeLevelsDepthDecoders.elm"))
    );
  });

  it('can use provided decoders' , () => {
    shell.exec("./elm-gen d ../tests_data/WithDecoder.elm .");
    expect(
      readFile(outPath("WithDecoderDecoders.elm"))
    ).to.equal(
      readFile(dataPath("WithDecoderDecoders.elm"))
    );
  });

  it('understands meta comments' , () => {
    shell.exec("./elm-gen d ../tests_data/MetaComments.elm .");
    expect(
      readFile(outPath("MetaCommentsDecoders.elm"))
    ).to.equal(
      readFile(dataPath("MetaCommentsDecoders.elm"))
    );
  });

  it('generates both decoders and encoders' , () => {
    shell.exec("./elm-gen d,e ../tests_data/Basic.elm .");
    expect(
      readFile(outPath("BasicDecodersAndEncoders.elm"))
    ).to.equal(
      readFile(dataPath("BasicDecodersAndEncoders.elm"))
    );
  });

  it('generates both decoders and encoders for dependent on other files types' , () => {
    shell.exec("./elm-gen d,e ../tests_data/DependentOnOtherFile.elm .");
    expect(
      readFile(outPath("DependentOnOtherFileDecodersAndEncoders.elm"))
    ).to.equal(
      readFile(dataPath("DependentOnOtherFileDecodersAndEncoders.elm"))
    );
  });

  it('generates both decoders and encoders for dependent types' , () => {
    shell.exec("./elm-gen d,e ../tests_data/DependentTypes.elm .");
    expect(
      readFile(outPath("DependentTypesDecodersAndEncoders.elm"))
    ).to.equal(
      readFile(dataPath("DependentTypesDecodersAndEncoders.elm"))
    );
  });

  it('generates both decoders and encoders for multi-arg union constructors' , () => {
    shell.exec("./elm-gen d,e ../tests_data/MetaComments.elm .");
    expect(
      readFile(outPath("MetaCommentsDecodersAndEncoders.elm"))
    ).to.equal(
      readFile(dataPath("MetaCommentsDecodersAndEncoders.elm"))
    );
  });

})
