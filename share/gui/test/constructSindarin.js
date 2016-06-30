const construct = require('../lib/constructSindarin.js');
const expect = require('chai').expect;

describe('SindarinAssignment', () => {
  it('should be constructable', () => {
    const testSindarinAssignment = new construct.SindarinAssignment('foo', 17);
  });
  it('should write the expected string', () => {
    const testSindarinAssignment = new construct.SindarinAssignment('foo', 17);
    expect(testSindarinAssignment.writeToSindarin()).to.equal('foo = 17');
  });
});

describe('SindarinCommand', () => {
  it('should just write the input string (no logic here)', () => {
    const testSindarinCommand = new construct.SindarinCommand('foo');
    expect(testSindarinCommand.writeToSindarin()).to.equal('foo');
  });
});

describe('SindarinWriteHeader', () => {
  it('should write a header', () => {
    const testSindarinWriteHeader = construct.sindarinWriteHeader();
    expect(testSindarinWriteHeader).to.contain('automatically generated');
  });
});
