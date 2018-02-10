import * as mocha from 'mocha';
import * as chai from 'chai';
import { parseGenCommand, GenCommand } from './Utils';

const expect  = chai.expect;

describe('Some utility funcs are working and', () => {
    describe('can parse generator command', () => {
        it('can understand simple char' , () => {
            expect(parseGenCommand("d")).to.equal(GenCommand.Decoders);
            expect(parseGenCommand("e")).to.equal(GenCommand.Encoders);
            expect(parseGenCommand("d&e")).to.equal(GenCommand.DecodersAndEncoders);
            expect(parseGenCommand("e&d")).to.equal(GenCommand.DecodersAndEncoders);
          });

        it('can understand full words' , () => {
            expect(parseGenCommand("decoders")).to.equal(GenCommand.Decoders);
            expect(parseGenCommand("encoders")).to.equal(GenCommand.Encoders);
            expect(parseGenCommand("decoders&encoders")).to.equal(GenCommand.DecodersAndEncoders);
            expect(parseGenCommand("encoders&decoders")).to.equal(GenCommand.DecodersAndEncoders);
          });
    });
});
      