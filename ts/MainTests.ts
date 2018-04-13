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

  it('for simple dependent types' , () => {
    shell.exec("./elm-gen d ../tests_data/DependentTypes.elm . ");
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

  it('can use provided decoders' , () => {
    shell.exec("./elm-gen d,e ../tests_data/WithDecoder.elm .");
    expect(
      readFile(outPath("WithDecoderDecodersAndEncoders.elm"))
    ).to.equal(
      readFile(dataPath("WithDecoderDecodersAndEncoders.elm"))
    );
  });

  it('can use provided encoders' , () => {
    shell.exec("./elm-gen d,e ../tests_data/WithEncoder.elm .");
    expect(
      readFile(outPath("WithEncoderDecodersAndEncoders.elm"))
    ).to.equal(
      readFile(dataPath("WithEncoderDecodersAndEncoders.elm"))
    );
  });

  it('can use both provided encoders & decoders' , () => {
    shell.exec("./elm-gen d,e ../tests_data/WithBoth.elm .");
    expect(
      readFile(outPath("WithBothDecodersAndEncoders.elm"))
    ).to.equal(
      readFile(dataPath("WithBothDecodersAndEncoders.elm"))
    );
  });

  it('for three-levels deep type structure' , () => {
    shell.exec("./elm-gen d,e ../tests_data/ThreeLevelsDepth.elm .");
    expect(
      readFile(outPath("ThreeLevelsDepthDecodersAndEncoders.elm"))
    ).to.equal(
      readFile(dataPath("ThreeLevelsDepthDecodersAndEncoders.elm"))
    );
  });

  it('transitive import issue' , () => {
    shell.exec("./elm-gen d ../tests_data/TransitiveImport.elm .");
    expect(
      readFile(outPath("TransitiveImportDecoders.elm"))
    ).to.equal(
      readFile(dataPath("TransitiveImportDecoders.elm"))
    );
  });

  it('uses provided config' , () => {
    shell.exec("./elm-gen d ../tests_data/Basic.elm . --config ../tests_data/elm-gen.json");
    expect(
      readFile(outPath("BasicWithConfig.elm"))
    ).to.equal(
      readFile(dataPath("BasicWithConfig.elm"))
    );
  });

  it('uses provided default values for decoders' , () => {
    shell.exec("./elm-gen d ../tests_data/WithDefaultValues.elm .");
    expect(
      readFile(outPath("WithDefaultValuesDecoders.elm"))
    ).to.equal(
      readFile(dataPath("WithDefaultValuesDecoders.elm"))
    );
  });

  it('uses generates decoder for nested module' , () => {
    shell.exec("./elm-gen d ../tests_data/Nested/Module.elm .");
    expect(
      readFile(outPath("ModuleDecoders.elm"))
    ).to.equal(
      readFile(dataPath("ModuleDecoders.elm"))
    );
  });
  
  it('can print record values' , () => {
    shell.exec("./elm-gen d ../tests_data/WithDefaultRecord.elm .");
    expect(
      readFile(outPath("WithDefaultRecordDecoders.elm"))
    ).to.equal(
      readFile(dataPath("WithDefaultRecordDecoders.elm"))
    );
  });

  it('can use provided decoders for imported types' , () => {
    shell.exec("./elm-gen d ../tests_data/WithDecoder2.elm .");
    expect(
      readFile(outPath("WithDecoder2Decoders.elm"))
    ).to.equal(
      readFile(dataPath("WithDecoder2Decoders.elm"))
    );
  });

});