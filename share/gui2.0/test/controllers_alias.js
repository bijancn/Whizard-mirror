require('babel-polyfill');
const alias = require('../controllers/alias.js');
const expect = require('chai').expect;
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
chai.use(chaiAsPromised);

describe('SindarinAlias', () => {
  const testAliasName = 'quark';
  const testAlias = 'u:U';
  it('should be constructable', () => {
    const sindarinAlias = new alias.SindarinAlias(testAliasName, testAlias);
    expect(sindarinAlias.alias).to.equal(testAlias);
    expect(sindarinAlias.alias).to.equal(testAlias);
  });
  it('should write to a string', () => {
    const sindarinAlias = new alias.SindarinAlias(testAliasName, testAlias);
    expect(sindarinAlias.toString()).to.equal('alias quark = u:U');
  });
});
