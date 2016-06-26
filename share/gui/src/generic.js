const guiconfig = require('./guiconfig');

// Ability to remove specific type of elements from the array
// array.remove('like this').remove('and like this');
// TODO: (bcn 2016-03-25) more standard way to do this?
Array.prototype.remove = () => { // eslint-disable-line no-extend-native
  let what;
  let ax;
  const a = arguments;
  let L = a.length;
  while (L && this.length) {
    what = a[--L];
    while ((ax = this.indexOf(what)) !== -1) { // eslint-disable-line no-cond-assign
      this.splice(ax, 1);
    }
  }
  return this;
};


// Removes duplicates in array
// TODO: (bcn 2016-03-25) more standard way to do this?
export function arrayUnique(array) {
  const a = array.concat();
  for (let i = 0; i < a.length; ++i) {
    for (let j = i + 1; j < a.length; ++j) {
      if (a[i] === a[j]) {
        a.splice(j--, 1);
      }
    }
  }
  return a;
}


// Ex: W+ used in GUI will be replaced with "W+" in sindarin file
// (Only for cuts atm)
// TODO: (bcn 2016-03-25) not even remotely generic
export function parseParticleName(name) {
  if (name === 'W+') return '"W+"';
  if (name === 'W-') return '"W-"';
  if (name === 'e+') return '"e+"';
  if (name === 'e-') return '"e-"';
  return name;
}


// TODO: (bcn 2016-03-25) not even remotely generic
export function parseParticleNameString(str) {
  const FromTo = [
    ['e+', '"e+"'],
    ['e-', '"e-"'],
    ['W+', '"W+"'],
    ['W-', '"W-"'],
    ['w+', '"W+"'],
    ['w-', '"W-"'],
  ];
  let newStr = str;
  for (let n = 0; n < FromTo.length; n++) {
    newStr = newStr.replace(FromTo[n][0], FromTo[n][1]);
  }
  return newStr;
}


// Construct latex out of def
// Experimental.
function constructTex(tex) {
  const newTex = tex.replace('=>', '\\Rightarrow')
    .replace(/\+/g, '^%2B')
    .replace(/\-/g, '^-')
    .replace(/"/g, '');
  return newTex;
}


// Using Google Chart API to get Latex images
function getLatexImage(tex) {
  const newTex = tex.replace(/\+/g, '%2B').replace(/ /g, '%20');
  return 'http://chart.apis.google.com/chart?cht=tx&chf=bg,s,FFFFFF00&chl=' +
    newTex;
}


// This function returns latex image of string str1 if useGoogleLatex
// is set to true, or str2 otherwise.
// Sometimes user may not have internet connection and latex images could
// not be generated using getLatexImage(s).
function image(str1, str2) {
  if (guiconfig.context.useGoogleLatex) {
    return '<img src="' + getLatexImage(str1) + '">';
  }
  return str2;
}


export function texImageOrPlain(name) {
  return image(constructTex(name), name);
}


export function htmlEscape(str) {
  return String(str)
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}


export function getFileTimestamp(file) {
  $.post('/checktimestamp', {filename: file}, (data) => {
    console.warn(data);
  });
}


function getFileTimestampAsync(file) {
  return $.ajax({
    type: 'POST',
    url: '/checktimestamp',
    data: {filename: file},
    success: (data) => {console.warn(data);},
  });
}


function monitorHistogramChangesFromCheck(lastCheck, whizRunning) {
  if (!whizRunning) return;
  console.warn('^-.-^');
  const obj = getFileTimestampAsync('output-whiz/whizard_analysis.pdf');
  obj.success((data) => {
    const thisCheck = new Date(data);
    const diff = Math.abs(lastCheck - thisCheck) / 1000;
    // Redisplay histogram if timestamp checked last time and timestamp check
    // this time differs
    if (diff > 0) {
      console.warn(diff);
      $('#out_hist').html('<embed src="whizard_analysis.pdf" width="100%" height="700px">');
    }
    setTimeout(() => {monitorHistogramChangesFromCheck(thisCheck, whizRunning);}, 10000);
  });
}


// Monitor for changes in output-whiz/whizard_analysis.pdf, if timestamp
// differences detected redisplay histogram.
export function monitorHistogramChanges(whizRunning) {
  const obj = getFileTimestampAsync('output-whiz/whizard_analysis.pdf');
  obj.success((data) => {
    const thisCheck = new Date(data);
    monitorHistogramChangesFromCheck(thisCheck, whizRunning);
  });
}


// TODO: (bcn 2016-06-26) refactor with monitorHistogramChangesFromCheck
function monitorLogChangesFromCheck(lastCheck, whizRunning) {
  if (!whizRunning) return;
  console.warn('^-.-^ log');
  const obj = getFileTimestampAsync('output-whiz/whizard.log');
  obj.success((data) => {
    const thisCheck = new Date(data);
    const diff = Math.abs(lastCheck - thisCheck) / 1000;
    // Redisplay histogram if timestamp checked last time and timestamp check
    // this time differs
    if (diff > 0) {
      console.warn(diff);
      $('#whizoutput').load('whizard.log').fadeIn('fast');
      $('.outputcontainer').fadeIn('fast');
    }
    setTimeout(() => {monitorLogChangesFromCheck(thisCheck, whizRunning);}, 5000);
  });
}


// Monitor whizard.log during computation
export function monitorLogChanges(whizRunning) {
  const obj = getFileTimestampAsync('output-whiz/whizard.log');
  obj.success((data) => {
    const thisCheck = new Date(data);
    monitorLogChangesFromCheck(thisCheck, whizRunning);
  });
}
