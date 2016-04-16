/*
 *  Ability to remove specific type of elements from the array
 *  array.remove("like this").remove("and like this");
 */
// TODO: (bcn 2016-03-25) more standard way to do this?
Array.prototype.remove = function() {
  var what, a = arguments, L = a.length, ax;
  while (L && this.length) {
    what = a[--L];
    while ((ax = this.indexOf(what)) !== -1) {
      this.splice(ax, 1);
    }
  }
  return this;
};

/*
 * Removes duplicates in array
 */
// TODO: (bcn 2016-03-25) more standard way to do this?
function arrayUnique(array) {
  var a = array.concat();
  for(var i=0; i<a.length; ++i) {
    for(var j=i+1; j<a.length; ++j) {
      if(a[i] === a[j])
        a.splice(j--, 1);
    }
  }
  return a;
}

/*
 * Ex: W+ used in GUI will be replaced with "W+" in sindarin file
 * (Only for cuts atm)
 */
// TODO: (bcn 2016-03-25) not even remotely generic
function parseParticleName(name)
{
  if (name == 'W+') return '"W+"';
  if (name == 'W-') return '"W-"';
  if (name == 'e+') return '"e+"';
  if (name == 'e-') return '"e-"';
  return name;
}

// TODO: (bcn 2016-03-25) not even remotely generic
function parseParticleNameString(str)
{
  var FromTo = [
    ['e+', '"e+"'],
    ['e-', '"e-"'],
    ['W+', '"W+"'],
    ['W-', '"W-"'],
    ['w+', '"W+"'],
    ['w-', '"W-"']
  ];

    for(var n = 0; n < FromTo.length; n++)
      str = str.replace(FromTo[n][0], FromTo[n][1]);

    return str;
}

/*
 * Construct latex out of def
 * Experimental.
 */
function constructTex(tex)
{
  tex = tex.replace('=>', '\\Rightarrow')
    .replace(/\+/g, '^%2B')
    .replace(/\-/g, '^-')
    .replace(/"/g, '');

  return tex;
}

/*
 * Using Google Chart API to get Latex images
 */
function getLatexImage(tex)
{
  tex = tex.replace(/\+/g, '%2B');
  tex = tex.replace(/ /g, '%20');
  return 'http://chart.apis.google.com/chart?cht=tx&chf=bg,s,FFFFFF00&chl=' + (tex);
}

/*
 * This function returns latex image of string str1 if USE_GOOGLE_LATEX
 * is set to true, or str2 otherwise.
 * Sometimes user may not have internet connection and latex images could
 * not be generated using getLatexImage(s).
 */
// TODO: (bcn 2016-03-25) is offline use really handled correctly?
function T(str1, str2)
{
  if(MGUI.USE_GOOGLE_LATEX == true)
    return '<img src="'+getLatexImage(str1)+'">';
  else
    return str2;
}

function htmlEscape(str) {
  return String(str)
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

function getFileTimestamp(file)
{
  $.post('/checktimestamp', { filename: file }, function(data) {
    console.log(data);
  });
}

function getFileTimestampAsync(file) {
  return $.ajax({
    type: "POST",
    url: "/checktimestamp",
    data: {filename: file},
    success: function(data) { }
  });
}

/*
 * Monitor for changes in output-whiz/whizard_analysis.pdf, if timestamp
 * differences detected redisplay histogram.
 */
function MonitorHistogramChanges() {

  var obj = getFileTimestampAsync("output-whiz/whizard_analysis.pdf");

  obj.success(function (data) {
    var thisCheck = new Date(data);
    MonitorHistogramChanges(thisCheck);
  });
}

function MonitorHistogramChanges(lastCheck) {

  console.log("^-.-^");

  var WhizRunning = 1;
  if (!WhizRunning) return;

  lastCheck = new Date(lastCheck); //probably unnecessary

  var obj = getFileTimestampAsync("output-whiz/whizard_analysis.pdf");

  obj.success(function (data) {
    var thisCheck = new Date(data);
    var diff = Math.abs(lastCheck - thisCheck)/1000;

    /*
     * Redisplay histogram if timestamp checked last time and timestamp check this time differs
     */
    if (diff > 0) {
      console.log(diff);
      $("#out_hist").html('<embed src="whizard_analysis.pdf" width="100%" height="700px">');
    }

    // Check every 10000 ms.
    setTimeout(function(){ MonitorHistogramChanges(thisCheck); }, 10000);
  });
}

/*
 * Monitor whizard.log during computation
 */
function MonitorLogChanges() {

  var obj = getFileTimestampAsync("output-whiz/whizard.log");

  obj.success(function (data) {
    var thisCheck = new Date(data);
    MonitorLogChanges(thisCheck);
  });
}

function MonitorLogChanges(lastCheck) {

  if (!WhizRunning) return;
  console.log("^-.-^ log");

  lastCheck = new Date(lastCheck); //probably unnecessary

  var obj = getFileTimestampAsync("output-whiz/whizard.log");

  obj.success(function (data) {
    var thisCheck = new Date(data);
    var diff = Math.abs(lastCheck - thisCheck)/1000;

    /*
     * Redisplay histogram if timestamp checked last time and timestamp check this time differs
     */
    if (diff > 0) {
      console.log(diff);
      $("#whizoutput").load( "whizard.log" ).fadeIn("fast");
      $(".outputcontainer").fadeIn("fast");
    }

    // Check every 10000 ms.
    setTimeout(function(){ MonitorLogChanges(thisCheck); }, 5000);
  });
}
