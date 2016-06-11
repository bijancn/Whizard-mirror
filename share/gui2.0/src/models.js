function SindarinParameterToString() {
  return this.name + ' = ' + this.value;
}

function SindarinParameter(name, value) {
  this.name = name;
  this.value = value;
  this.toString = SindarinParameterToString;
}

function SindarinWriteModelData () {
  for (var i=0; i<this.nElements; i++) {
    p = this.list[i];
    if (p instanceof SindarinModelData) {
      this.src += p.model.toString() + "\n";
      for (var j=0; j<p.parameters.length; j++) {
        this.src += p.parameters.toString() + "\n";
      }
      this.elementsUsed.push(i);
    }
  }
}

function SindarinModelData(model) {
  this.model = model;
  this.parameters = [];
  this.writeModelData = SindarinWriteModelData;
}

function SindarinModelToString() {
  return 'model = ' + this.modelName;
}

function SindarinModel(name, description) {
  this.modelName = name;
  this.description = description;
  this.toString = SindarinModelToString;
}

function fillModelList() {
  const modelList = [];
  let modelName;
  let description;

  modelName = '2HDM';
  description = 'Two-Higgs Doublet Model';
  modelList.push(new SindarinModel(modelName, description));

  modelName = '2HDM_CKM';
  description = 'Two-Higgs Doublet Model with CKM matrix';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'AltH';
  description = 'An SM extension for VV scattering';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'GravTest';
  description = 'SQED with gravitino';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'HSExt';
  description = '?????';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'Littlest';
  description = 'Littles Higgs';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'Littlest_Eta';
  description = 'Littlest Higgs with ungauged U(1)';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'Littlest_Tpar';
  description = 'Littlest Higgs with T parity';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'MSSM';
  description = 'Minimal supersymmetric standard model';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'MSSM_CKM';
  description = 'MSSM with CKM matrix';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'MSSM_Grav';
  description = 'MSSM with gravitinos';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'MSSM_Hgg';
  description = 'MSSM with Hgg-coupling (???)';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'NMSSM';
  description = 'Next-to-Minimal supersymmetric standard model';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'NMSSM_CKM';
  description = 'NMSSM with CKM matrix';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'NMSSM_Hgg';
  description = 'NMSSM with Hgg-coupling (???)';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'NoH_rx';
  description = 'SM with anomalous Higgs couplings';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'PSSSM';
  description = 'Extended SUSY models';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'QCD';
  description = 'QCD with d,u,s,c,b,t,g';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'QED';
  description = 'QED with e,μ,τ,γ';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'Simplest';
  description = 'Simplest Little Higgs (anomaly-free)';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'Simplest_univ';
  description = 'Simplest Little Higgs (universal)';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM';
  description = 'Standard model';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM_ac';
  description = 'SM with anomalous gauge couplings';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM_ac_CKM';
  description = 'SM with anomalous gauge couplings and CKM matrix';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM_CKM';
  description = 'SM with CKM matrix';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM_hadrons';
  description = '???';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM_Higgs';
  description = 'SM with Hgg, Hγγ and Hμμ';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM_rx';
  description = 'SM with anomalous Higgs couplings';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM_top';
  description = 'SM with chareg 4/3 top';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM_top_anom';
  description = 'SM with anomalous top coupings';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM_tt_threshold';
  description = 'SM with top-threshold resummation';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SM_ul';
  description = 'SM with anomalous Higgs couplings';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SSC';
  description = 'SM extension for VV scattering';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SSC_2';
  description = 'SM extension for VV scattering';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'SSC_AltT';
  description = 'SM extension for VV scattering';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'Template';
  description = 'Augmentable SM template';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'Threeshl';
  description = '???';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'Threeshl_nohf';
  description = '???';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'UED';
  description = 'Universal Extra Dimensions';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'Xdim';
  description = 'SM with graviton';
  modelList.push(new SindarinModel(modelName, description));

  modelName = 'Zprime';
  description = "SM with Z'";
  modelList.push(new SindarinModel(modelName, description));

  return modelList;
}

module.exports = {SindarinWriteModelData, fillModelList, SindarinModelData};
