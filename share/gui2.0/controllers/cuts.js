function SindarinCutsToString () {
  return "cuts = " + this.CutsData;
}

function SindarinCuts(cuts) {
  this.CutsData = cuts;
  this.toString = SindarinCutsToString;
}

function SindarinWriteCuts () {
  for(var i = 0; i < this.list.length; i++) {
    var elem = this.list[i];
    if (elem instanceof SindarinCuts) {
      if (elem.CutsData.length > 0) {
        this.src += elem.toString() + '\n';
      }
    }
  }
}

var cuts = (function() {
  // private variables
  var Public = {};
  var LastClickedCutName;
  var LastClickedCutEq;
  var ActiveInputParticleElement = null;
  var CutNames = ['Pt', 'E', 'M', 'M2', 'abs(Eta)', 'abs(cos(Theta))'];
  var CutEq = ['>', '<'];

  //Public methods

  /*
   * ActiveInputElement (particles in cuts)
   */
  Public.setLastActiveInputElement = function(element) {
    ActiveInputParticleElement = element;
  };

  Public.getLastActiveInputElement = function() {
    return ActiveInputParticleElement;
  };

  /*
   * LastClickedCutName(name of cut)
   */
  Public.setLastClickedCutName = function(element) {
    LastClickedCutName = element;
  };

  Public.getLastClickedCutName = function() {
    return LastClickedCutName;
  };

  /*
   * LastClickedCutEq (inequality in cut)
   */
  Public.setLastClickedCutEq = function(element) {
    LastClickedCutEq = element;
  };

  Public.getLastClickedCutEq = function() {
    return LastClickedCutEq;
  };

  /*
   * getActiveParticles: Returns array of particles used in GUI
   */
  Public.getActiveParticles = function() {
    var ParticlesList = [];

    /* Get particles from allias list */
    for(var i=0; i < ExternalSindarinList.length; i++) {
      if (ExternalSindarinList[i] instanceof SindarinAlias) {
        var alias = ExternalSindarinList[i].alias;
        ParticlesList = arrayUnique(ParticlesList.concat(alias.split(':')));
      }
    }

    /* Construct particle list from process definitions */
    for(var i = 0; i < ProcessList.length; i++) {
      if (ProcessList[i] === null) continue; // Check if process was removed
      var Process = ProcessList[i].incoming + ' ' + ProcessList[i].outgoing;
      Process = Process.replace(/"/g, "").replace(/'/g, "").replace(/\(|\)/g, "").replace(/,/g, ' ');
      Process = Process.split(' ').filter(function(n){ return n != "" });
      ParticlesList = arrayUnique(ParticlesList.concat(Process));
    }

    return ParticlesList;
  };

  /*
   * getCutsArray: Returns array of used cuts in sindarin format
   */
  Public.getCutsArray = function() {

    var CutsList = [];

    $('[rel=cut-elem]').each(function(i, obj) {
      var cutName = $(this).find(".cut-name").text().replace(/ /g,'');
      var cutEq = $(this).find(".cut-eq").text().replace(/ /g,'');
      var cutVal = $(this).find(".cut-val").val();
      var cutAssignment = $(this).find(".cut-assignment").val();

      /* Contains an array of individual particles used in Cut */
      if (cutAssignment === undefined) return;

      cutAssignment = cutAssignment.split(' ').remove(" ").remove("");

      /* Constructing string of form: p1,p2,p3... */
      if(cutAssignment.length && cutVal.length && cutEq.length && cutName.length) {

        var ParticlesString = "";
        for(var i = 0; i < cutAssignment.length; i++)
          ParticlesString += parseParticleName(cutAssignment[i]) + ':';
        ParticlesString = ParticlesString.slice(0, -1);

        /* Cut format going into Whizard */
        CutsList.push("all " + cutName + " " + cutEq + " " + cutVal + " [collect[" + ParticlesString + "]]");
        //console.log("all " + cutName + " " + cutEq + " " + cutVal + " [collect[" + ParticlesString + "]]");
      }
    });

    return CutsList;
  };

  /*
   * RebuildParticlesHTML: Rebuilds particles list html code in Cuts tab
   */
  Public.RebuildParticlesHTML = function() {

    /* Cleaning current list and building new */
    $("#cuts-html-particles-list").html('');
    var particles = cuts.getActiveParticles();
    for(var i = 0; i < particles.length; i++) {
      $("#cuts-html-particles-list").append('<li role="presentation"><a href="#" class="cuts-particles-click">' + particles[i] + '</a></li>');
    }
  };

  /*
   * Clean: Cleans all cuts data
   */
  Public.Clean = function() {
    $("#cutsContainer").html("");
  };

  Public.AddNewCut = function(name, eq, cutValue, cutAssignment) {
    $("#cutsContainer").append(Public.getCutHTML(name, eq, cutValue, cutAssignment));
  };

  Public.getCutHTML = function(name, eq, cutValue, cutAssignment) {
    var CutCode = '<div class="row" rel="cut-elem"> \
                  <div class="col-md-12"> \
                  <div class="btn-group"> \
                  <button type="button" class="btn btn-default dropdown-toggle cut-name" data-toggle="dropdown"> ' + name + ' <span class="caret"></span> \
                  </button> \
                  <ul class="dropdown-menu scrollable-menu" role="menu">';

    for(var i = 0; i < CutNames.length; i++)
      CutCode += '<li><a href="#" class="cuts-select-name">' + CutNames[i] + '</a></li>';

    CutCode += '<li><a href="#" class="cuts-select-delete"><span class="glyphicon glyphicon-remove-sign" aria-hidden="true"></span> Delete</a></li></ul> \
               </div> \
               <div class="btn-group"> \
               <button type="button" class="btn btn-default dropdown-toggle cut-eq" data-toggle="dropdown">' + eq + ' <span class="caret"></span> \
               </button> \
               <ul class="dropdown-menu" role="menu">';

    for(var i = 0; i < CutEq.length; i++)
      CutCode += '<li><a href="#" class="cuts-select-eq">' + CutEq[i] + '</a></li>';

    CutCode += '</ul> \
               </div> \
               \
               <div class="btn-group"> \
               <input type="text" class="form-control cut-val" placeholder="500 GeV" value="' + cutValue + '"> \
               </div> \
               \
               <div class="btn-group pull-right"> \
               <form class="form-inline"> \
               <div class="form-group"> \
               <label for="exampleInputName2">Union:</label> \
               <input type="text" rel="" class="form-control cuts-particles-active cut-assignment" value="' + cutAssignment + '"> \
               </div> \
               </form> \
               </div> \
               </div> \
               </div>';

    return CutCode;
  };

  //Return just the public parts
  return Public;
}());


/*
 * Clicking on Cuts->Particles
 * Input is added for the last active input field
 */
$(document).on("click", ".cuts-particles-click", function() {
  //var oldList = ActiveInputParticleElement.val();
  var oldList = cuts.getLastActiveInputElement().val();
  cuts.getLastActiveInputElement().val(oldList + ' ' + $(this).text());
});

/*
 *  Clicking on the input field making it active
 */
$(document).on("click", ".cuts-particles-active", function() {
  //ActiveInputParticleElement = $(this);
  cuts.setLastActiveInputElement($(this));
});

/*
 *  Checking focus to show/hide particle Cuts -> Particles List
 */
$(document).on("click", "body", function() {
  if ($(".cuts-particles-active").is(":focus") || $(".cuts-particles-click").is(":focus"))
    $("#cuts-html-particles").fadeIn("fast");
  else
    $("#cuts-html-particles").fadeOut("fast");

});

/*
 * Selecting cut name (Pt, M)
 */
$(document).on("click", ".cut-name", function() {
  cuts.setLastClickedCutName($(this));
});

$(document).on("click", ".cuts-select-name", function() {
  cuts.getLastClickedCutName().html($(this).text() + ' <span class="caret"></span>');
});

/*
 * Selecting cut inequality sign (>, <)
 */
$(document).on("click", ".cut-eq", function() {
  cuts.setLastClickedCutEq($(this));
});

$(document).on("click", ".cuts-select-eq", function() {
  cuts.getLastClickedCutEq().html($(this).text() + ' <span class="caret"></span>');
});

/*
 * Selecting to remove cut
 */
$(document).on("click", ".cuts-select-delete", function() {
  cuts.getLastClickedCutName().parent().parent().unbind().remove();
});

/*
 * Button: Cuts > New Cut
 */
$(".cuts-newcut").click(function() {
  //default
  cuts.AddNewCut('Pt', '>', '', '');
});

