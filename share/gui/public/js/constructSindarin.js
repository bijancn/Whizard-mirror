function SindarinAssignment(str, val) {
	this.identifier = str;
	this.value = val;
	this.toString = SindarinAssignmentToString;
}

function SindarinAssignmentToString () {
	return this.identifier + " = " + this.value;
}

function SindarinCommand(str) {
	this.command = str;
	this.toString = SindarinCommandToString;
}

function SindarinCommandToString () {
	return this.command;
}

function SindarinGenerator (SindarinList) {
	this.list = SindarinList;
	this.src = "";
	this.nElements = SindarinList.length;
	// We will use this array to save the indices of the elements which have already been processed.
	this.elementsUsed = new Array ();
	this.construct = ConstructSindarinFromList;
	this.writeHeader = SindarinWriteHeader;
	this.writeModelData = SindarinWriteModelData;
	this.writeAdditionalData = SindarinWriteAdditionalCode;
	this.writeAliases = SindarinWriteAliases;
	this.writeProcesses = SindarinWriteProcesses;
	this.writeBody = SindarinWriteBody;
	this.writeSimulate = SindarinWriteSimulate;
	this.writeCuts = SindarinWriteCuts;
}

function SindarinWriteHeader () {
    this.src += "# This Sindarin script was automatically generated by the Whizard-GUI\n";
	var date = new Date();
	this.src += "# Date: " + (date.getMonth()+1) + "-" + date.getDate() + "-" +  date.getFullYear(); 
	this.src += ", " + date.getHours() + ":" + date.getMinutes() + "h\n";
	this.src += "\n";
}

/* 
 * For additional code section
 */
function SindarinAdditionalCode (s) {
	this.as = s;
	this.writeModelData = SindarinWriteAdditionalCode;
}
 
function SindarinWriteAdditionalCode() {
	for(var i = 0; i < this.list.length; i++) {
		var elem = this.list[i];
		if (elem instanceof SindarinAdditionalCode) {
			this.src += elem.as;
			this.src += '\n';
		}
	}
}



function SindarinWriteBody () {
	for (var i=0; i<this.nElements; i++) {
		if (this.elementsUsed.indexOf(i) < 0) {
			this.src += this.list[i].toString() + "\n";
			this.elementsUsed.push(i);
		}
	}
}



function ConstructSindarinFromList() {
	// First, the header
	this.writeHeader();
	this.writeAdditionalData();
	// Models and parameters must be early in the Sindarin
	this.writeModelData();
	// Then the aliases. 
	this.writeAliases();
	this.writeCuts();
	this.writeProcesses();
	this.writeSimulate();
	//Lastly, the body
	//this.writeBody(); // adds stuff that should not be there
	
	return this.src;
}

function showAliases(SindarinList) {
	var n = SindarinList.length;
	for (var i=0; i<n; i++) {
		if (SindarinList[i] instanceof SindarinAlias) {
			console.log(SindarinList[i].toString());
		}
	}
}