	/* 
	 * Contains Aliases data 
	 */
	 
	var ProcessList = [];
		 
	function SindarinProcess(incoming, outgoing) {
		this.counter = 0;
		this.incoming = incoming;
		this.outgoing = outgoing;
		this.integrationData = new integrationData ();
		this.simulationData = new SimulationData ();
		this.ScanData = new ScanData();
		this.isNlo = SindarinProcessIsNlo;
		this.setNlo = SindarinProcessSetNlo;
		this.setSqrts = SindarinProcessSetSqrts;
		this.getSqrts = SindarinProcessGetSqrts;
		this.getNIter = SindarinProcessGetNIter;
		this.setNIter = SindarinProcessSetNIter;
		this.getNCalls = SindarinProcessGetNCalls;
		this.setNCalls = SindarinProcessSetNCalls;
		this.getNEvents = SindarinProcessGetNEvents;
		this.setNEvents = SindarinProcessSetNEvents;
		this.toString = SindarinProcessToString;
		this.Name = ProcessDisplayName;
		this.grabScanData = grabScanData;
	}

	function ExtAssignScans() {
		for(var i = 0; i < ProcessList.length; i++) {
			ProcessList[i].grabScanData(i);
		}
	}
	
	function SindarinProcessIsNlo () {
		return this.integrationData.nlo;
	}

	function SindarinProcessSetNlo (value) {
		this.integrationData.nlo = value;
	}

	function SindarinProcessGetSqrts () {
		return this.integrationData.sqrts;
	}

	function SindarinProcessSetSqrts (sqrts) {
		this.integrationData.sqrts = sqrts;
	}

	function SindarinProcessGetNIter () {
		return this.integrationData.nIter;
	}

 	function SindarinProcessSetNIter (nIt) {
		this.integrationData.nIter = nIt;
	}

	function SindarinProcessGetNCalls () {
		return this.integrationData.nCalls;
	}

	function SindarinProcessSetNCalls (nCalls) {
		this.integrationData.nCalls = nCalls;
	}

	function SindarinProcessGetNEvents () {
		return this.simulationData.nEvents;
	}

	function SindarinProcessSetNEvents (nEv) {
		this.simulationData.nEvents = nEv;
	}

	function SindarinProcessToString () {
		try {
			if (this.counter <= 0) throw "Invalid process counter";
			var str = "process proc_" + this.counter + " = " + this.incoming + " => " + this.outgoing;
			if (this.isNlo()) str += ' {nlo_calculation = "Full"}';
			return str;
		}
		catch (err) {
			console.log (err)
		}
	}
	
	function ProcessDisplayName() {
		return 'proc_' + this.counter + ' = ' + this.incoming + " => " + this.outgoing;
	}

	function integrationData () {
		this.nlo = false;
		//Default values, could be set in a file
		this.sqrts = 0;
		this.nCalls = 10000;
		this.nIter = 5;
	}
	
	function ScanData() {
		this.Sets = [];
		this.type;
		this.title;
		this.xlabel;
		this.ylabel;
		this.xmin;
		this.xmax;
	}
	
	function grabScanData(processID) {
		this.ScanData.Sets = ScansList[processID].ScansContainer;
		this.ScanData.type = ScansList[processID].type;
		this.ScanData.title = ScansList[processID].title;
		this.ScanData.xlabel = ScansList[processID].xlabel;
		this.ScanData.ylabel = ScansList[processID].ylabel;
		this.ScanData.xmin = ScansList[processID].xmin;
		this.ScanData.xmax = ScansList[processID].xmax;
	}

	function SimulationData () {
		this.nEvents = 0;
	}


	function SindarinWriteProcesses () {	
		for (var i=0; i<this.nElements; i++) {
			p = this.list[i];
			if (p instanceof SindarinProcess) {
				this.src += this.list[i].toString() + "\n";

				//If scans defined overwrite
				if (p.ScanData.Sets.length > 0) {
					console.log(p.ScanData.Sets.length);
					this.src += '#Plot data' + '\n';
					this.src += '$x_label = "'+p.ScanData.xlabel+'"' + '\n';
					this.src += '$y_label = "'+p.ScanData.ylabel+'"' + '\n';
					this.src += '$title = "'+p.ScanData.title+'"' + '\n';
					this.src += 'plot lineshape_' + i + ' { x_min = ' +p.ScanData.xmin + ' x_max = ' + p.ScanData.xmax + ' }' + '\n';
					
					this.src += 'scan sqrts = (';
					for(var j = 0; j < p.ScanData.Sets.length; j++) {
						var e = p.ScanData.Sets[j];
						this.src += '('+e.min+' => '+e.max+' /+ '+e.inc+'),';
					}
					
					//@.@
					this.src = this.src.substring(0, this.src.length -1);
						
					this.src += ') {' + '\n';
					this.src += '\tintegrate (proc_'+p.counter+') { iterations = 2:1000:"gw", 1:2000 }' + '\n';
					this.src += 'record lineshape_' + i + '(sqrts, integral (proc_'+i+') / 1000)' + '\n';
					this.src += '}' + '\n';
					this.src += 'compile_analysis '; //{ $out_file = "AUG.dat" }
				} else {
					this.src += "sqrts = " + this.list[i].getSqrts() + "\n";
					if (p.getNCalls() > 0 && p.getNIter() > 0) {
						this.src += 'integrate(proc_' + p.counter 
						   + ') {iterations=' + p.getNIter() + ':' 
						   + p.getNCalls() + ':"gw"}\n';
					}
				}
				if (p.getNEvents() > 0) {
					this.src += 'simulate(proc_' + p.counter + 
					   ') {n_events=' + p.getNEvents() + '}\n';
				}
				this.elementsUsed.push(i);
			}
		}
		console.log(this.src);
	}

	function rebuildProcessList() {
		$("#pop_process").empty();
		$("#pop_process").append('<div class="row">');
		proc_index = 1;
		for(var i=0; i < ProcessList.length; i++) {
			if (ProcessList[i] instanceof SindarinProcess) {
				if (ProcessList[i] === null) continue; //!!!
					ProcessList[i].counter = proc_index;
					proc_index++;
					$("#pop_process").append('<div class="col-md-10"><a href="javascript:;" class="process">' + ProcessList[i].Name() + '</a></div><div class="col-md-2"><a href="javascript:;" class="process-remove" process-id=' + i + '><span class="glyphicon glyphicon-remove-sign" aria-hidden="true"></span></a></div>');
			}
			$("#pop_process").append('</div>');
		}
	}

	/* 
	 * Add new process, useful with examples
	 */
	function AddProcess(incoming, outgoing) {
		ProcessList.push (new SindarinProcess(incoming, outgoing));
		AddSimulation();
		Scan.AddProcess();
		rebuildProcessList();
	}

	function CleanProcess() {
		ProcessList = [];
		rebuildProcessList();			
	}
	
