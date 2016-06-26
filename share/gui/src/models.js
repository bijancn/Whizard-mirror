export function SindarinWriteModelData() {
  for (let i = 0; i < this.nElements; i++) {
    const p = this.list[i];
    // TODO: (bcn 2016-06-14) both functions use each other
    // eslint-disable-next-line no-use-before-define
    if (p instanceof SindarinModelData) {
      this.src += p.model.toString() + '\n';
      for (let j = 0; j < p.parameters.length; j++) {
        this.src += p.parameters.toString() + '\n';
      }
      this.elementsUsed.push(i);
    }
  }
}


export function SindarinModelData(model) {
  this.model = model;
  this.parameters = [];
  this.writeModelData = SindarinWriteModelData;
}


function SindarinModelToString() {
  return 'model = ' + this.modelName;
}


export function SindarinModel(name, description) {
  this.modelName = name;
  this.description = description;
  this.toString = SindarinModelToString;
}


export function fillModelList() {
  const modelList = [];
  const models = [
    {name: '2HDM', description: 'Two-Higgs Doublet Model'},
    {name: '2HDM_CKM', description: 'Two-Higgs Doublet Model with CKM matrix'},
    {name: 'AltH', description: 'An SM extension for VV scattering'},
    {name: 'GravTest', description: 'SQED with gravitino'},
    {name: 'HSExt', description: '?????'},
    {name: 'Littlest', description: 'Littles Higgs'},
    {name: 'Littlest_Eta', description: 'Littlest Higgs with ungauged U(1)'},
    {name: 'Littlest_Tpar', description: 'Littlest Higgs with T parity'},
    {name: 'MSSM', description: 'Minimal supersymmetric standard model'},
    {name: 'MSSM_CKM', description: 'MSSM with CKM matrix'},
    {name: 'MSSM_Grav', description: 'MSSM with gravitinos'},
    {name: 'MSSM_Hgg', description: 'MSSM with Hgg-coupling (???)'},
    {name: 'NMSSM', description: 'Next-to-Minimal supersymmetric standard model'},
    {name: 'NMSSM_CKM', description: 'NMSSM with CKM matrix'},
    {name: 'NMSSM_Hgg', description: 'NMSSM with Hgg-coupling (???)'},
    {name: 'NoH_rx', description: 'SM with anomalous Higgs couplings'},
    {name: 'PSSSM', description: 'Extended SUSY models'},
    {name: 'QCD', description: 'QCD with d,u,s,c,b,t,g'},
    {name: 'QED', description: 'QED with e,μ,τ,γ'},
    {name: 'Simplest', description: 'Simplest Little Higgs (anomaly-free)'},
    {name: 'Simplest_univ', description: 'Simplest Little Higgs (universal)'},
    {name: 'SM', description: 'Standard model'},
    {name: 'SM_ac', description: 'SM with anomalous gauge couplings'},
    {name: 'SM_ac_CKM', description: 'SM with anomalous gauge couplings and CKM matrix'},
    {name: 'SM_CKM', description: 'SM with CKM matrix'},
    {name: 'SM_hadrons', description: '???'},
    {name: 'SM_Higgs', description: 'SM with Hgg, Hγγ and Hμμ'},
    {name: 'SM_rx', description: 'SM with anomalous Higgs couplings'},
    {name: 'SM_top', description: 'SM with chareg 4/3 top'},
    {name: 'SM_top_anom', description: 'SM with anomalous top coupings'},
    {name: 'SM_tt_threshold', description: 'SM with top-threshold resummation'},
    {name: 'SM_ul', description: 'SM with anomalous Higgs couplings'},
    {name: 'SSC', description: 'SM extension for VV scattering'},
    {name: 'SSC_2', description: 'SM extension for VV scattering'},
    {name: 'SSC_AltT', description: 'SM extension for VV scattering'},
    {name: 'Template', description: 'Augmentable SM template'},
    {name: 'Threeshl', description: '???'},
    {name: 'Threeshl_nohf', description: '???'},
    {name: 'UED', description: 'Universal Extra Dimensions'},
    {name: 'Xdim', description: 'SM with graviton'},
    {name: 'Zprime', description: "SM with Z'"},
  ];
  for (const model of models) {
    modelList.push(new SindarinModel(model.name, model.description));
  }
  return modelList;
}
