import * as mocha from 'mocha';
import * as chai from 'chai';
import {parseGenCommand, Decoders, Encoders, DecodersAndEncoders, getOutputFileName} from "./Utils"

const expect  = chai.expect;

describe('Some utility funcs are working and', () => {
    describe('can parse generator command', () => {
        it('can understand simple char' , () => {
            expect(parseGenCommand("d")).to.be.instanceof(Decoders);
            expect(parseGenCommand("e")).to.be.instanceof(Encoders);
            expect(parseGenCommand("d,e")).to.be.instanceof(DecodersAndEncoders);
            expect(parseGenCommand("e,d")).to.be.instanceof(DecodersAndEncoders);
          });

        it('can understand full words' , () => {
            expect(parseGenCommand("decoders")).to.be.instanceof(Decoders);
            expect(parseGenCommand("encoders")).to.be.instanceof(Encoders);
            expect(parseGenCommand("decoders,encoders")).to.be.instanceof(DecodersAndEncoders);
            expect(parseGenCommand("encoders,decoders")).to.be.instanceof(DecodersAndEncoders);
          });
    });

    it('gets correct output file name' , () => {
        expect(getOutputFileName("inputFile", new Decoders)).to.equal("inputFileDecoders.elm");
        expect(getOutputFileName("inputFile", new Encoders)).to.equal("inputFileEncoders.elm");
        expect(getOutputFileName("inputFile", new DecodersAndEncoders)).to.equal("inputFileDecodersAndEncoders.elm");
      });

    
});
      