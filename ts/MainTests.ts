import * as mocha from 'mocha';
import * as chai from 'chai';
import * as shell from 'shelljs';

const expect = chai.expect;
describe('My test tools', () => {

  it('just working' , () => {
    expect(3).to.equal(3);
  });

});
describe('My cli tool', () => {
  
    it('just emits what was passed to it' , () => {
      expect(shell.exec("./dist/main 'just a string'").stdout).to.equal('just a string');
    });
  
  })
