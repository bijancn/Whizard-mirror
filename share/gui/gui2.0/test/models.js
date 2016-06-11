const models = require('../lib/models.js');
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
chai.use(chaiAsPromised);

describe('sindarinModel', () => {
  const modelName = '2HDM';
  const description = 'Two-Higgs Doublet Model';
  it('should be constructable', () => {
    const test = new models.SindarinModel(modelName, description);
    return test !== null;
  });
});
