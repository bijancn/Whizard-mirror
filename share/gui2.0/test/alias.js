const alias = require('../lib/alias.js');
const expect = require('chai').expect;
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
chai.use(chaiAsPromised);
//require("jsdom").env("", function(err, window) {
  //if (err) {
    //console.error(err);
    //return;
  //}
 
  //const $ = require("jquery")(window);
//});

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
    //$('#pop_aliases').empty();
    //const something = new alias.CleanAlias();
    //something.rebuildAliasList();
  });
});
