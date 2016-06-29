const alias = require('../lib/alias.js');
const expect = require('chai').expect;
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
chai.use(chaiAsPromised);
GLOBAL.$ = require('jquery');
// const chaiJquery = require('chai-jquery');

describe('SindarinAlias', () => {
  const testAliasName = 'quark';
  const testAlias = 'u:U';
  it('should be constructable', () => {
    const sindarinAlias = new alias.SindarinAlias(testAliasName, testAlias);
    expect(sindarinAlias.alias).to.equal(testAlias);
    expect(sindarinAlias.alias).to.equal(testAlias);
  });
  it('should write to the expected string', () => {
    const sindarinAlias = new alias.SindarinAlias(testAliasName, testAlias);
    expect(sindarinAlias.toString()).to.equal('alias quark = u:U');
  });
  it('should use jquery to populate the aliases list', () => {
    alias.cleanAlias();
    alias.rebuildAliasList();
    // expect($('#pop_aliases').get).to.have.length.above(0);
    // expect($('#pop_aliases')).to.contain('text');
  });
});
