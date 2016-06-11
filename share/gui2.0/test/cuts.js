const cuts = require('../lib/cuts');
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
chai.use(chaiAsPromised);

describe('cuts.Instance', () => {
  it('should have active particles', () => {
    console.log(cuts.Instance);
    const test = cuts.Instance.getActiveParticles();
    return test !== null;
  });
});

