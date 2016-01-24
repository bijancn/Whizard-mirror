	/*
	 *  Hiding optional fields
	 */
	$("#struct-scan").hide();

	/*
	 * Object structure:
	 * ScansList[i] : contain all Scan information for a i'th process
	 * ScansList[i].ScansContainer : contains union of subintervals for a i'th process
	 * ScansList[i].ScansContainer[j] : contains j'th individual subinterval for i'th process
	 * 								    ex: (88.0 GeV => 90.0 GeV /+ 0.5 GeV)
	 */

 	var ScansList = [];

	function ScanElement(min, max, inc) {
		this.min = min;
		this.max = max;
		this.inc = inc;
	}

	function ScanProcess() {
		this.ScansContainer = [];
		this.status = false;
		this.type;
		this.title;
		this.xlabel;
		this.ylabel;
		this.xmin;
		this.xmax;

		this.addScanElement = addScanElement;
	}

	function addScanElement(min, max, inc) {
		this.ScansContainer.push(new ScanElement(min, max, inc));
	}

	function addScanProcess() {
		Scans.addScanProcess();
	}


var Scan = {

		AddProcess: function() {
			ScansList.push(new ScanProcess());
		},

		/*
		 * Rebuilding Scan configuration for a selected process
		 */
		FillHTMLFields: function() {
			Scan.Clean();

			for(var i = 0; i < ScansList[activeProcessId].ScansContainer.length; i++) {
				var elem = ScansList[activeProcessId].ScansContainer[i];
				Scan.AddNew(elem.min, elem.max, elem.inc, i);
			}

			$("#conf-scan-check").prop('checked', ScansList[activeProcessId].status);
			$("#scan-plot-title").val(ScansList[activeProcessId].title);
			$("#scan-plot-xlabel").val(ScansList[activeProcessId].xlabel);
			$("#scan-plot-ylabel").val(ScansList[activeProcessId].ylabel);
			$("#scan-plot-xmin").val(ScansList[activeProcessId].xmin);
			$("#scan-plot-xmax").val(ScansList[activeProcessId].xmax);

		},

	getScanCodeHTML: function(min, max, inc, sid) {
		var code = '<div class="row" rel="scan-elem" scanid="'+sid+'"> \
				<div class="col-sm-4"> \
					<div class="form-group"> \
						<input type="text" class="form-control conf-scan-min" id="" placeholder="Min value" value="'+min+'"> \
					</div> \
				</div>  \
				<div class="col-sm-4"> \
					<div class="form-group"> \
						<input type="text" class="form-control conf-scan-max" id="" placeholder="Max value" value="'+max+'"> \
					</div> \
				</div>         \
				<div class="col-sm-4"> \
					<div class="form-group"> \
						<input type="text" class="form-control conf-scan-inc" id="" placeholder="Increment" value="'+inc+'"> \
					</div> \
				</div>  \
			</div>';
		return code;
	},

	AddNew: function(min, max, inc, sid) {
		$("#scansContainer").append(Scan.getScanCodeHTML(min, max, inc, sid));
	},

	Clean: function() {
		$("#scansContainer").html("");
	}

};

	/*
	 * Selecting: Scan > Process
	 */
	$(document).on("click", ".process-entry-scan", function() {
		$(".scan-right").fadeIn("fast");

		$('.process-entry-scan').removeClass('active');
		$(this).addClass('active');

		activeProcessId = $(this).attr("process-id");

		Scan.FillHTMLFields();

		if (ScansList[activeProcessId].status)
			$("#struct-scan").fadeIn("fast");
		else
			$("#struct-scan").fadeOut("fast");

	});

	/*
	 * Button: New Scan subinterval
	 */
	$(".scan-newscan").click(function() {
		Scan.AddNew('', '', '', ScansList[activeProcessId].ScansContainer.length);
		ScansList[activeProcessId].addScanElement(0,0,0);
	});

	/*
	 * Button: Clean Scans
	 */
	$(".scan-clean").click(function() {
		ScansList[activeProcessId].ScansContainer = [];
		Scan.Clean();
	});

	/*
	 * Checkbox button: Enable scan for this process
	 */
	$("#conf-scan-check").click(function() {
		ScansList[activeProcessId].status = $(this).prop('checked');

		if ($(this).prop('checked'))
			$("#struct-scan").fadeIn("fast");
		else
			$("#struct-scan").fadeOut("fast");


		/*
		 * Changing On/Off indicator
		 */
		if (ScansList[activeProcessId].status)
			$("#proc_indicator_scan_" + activeProcessId).removeClass("label-default label-success").addClass("label-success").text("On");
		else
			$("#proc_indicator_scan_" + activeProcessId).removeClass("label-default label-success").addClass("label-default").text("Off");
	});

	/*
	 * Modify field: Scan-minimum value
	 */
	$(document).on("change", ".conf-scan-min", function()	{
		var min = $(this).val();
		var intervalID = $(this).parent().parent().parent().attr('scanid');
		ScansList[activeProcessId].ScansContainer[intervalID].min = min;
	});

	/*
	 * Modify field: Scan-maximum value
	 */
	$(document).on("change", ".conf-scan-max", function()	{
		var max = $(this).val();
		var intervalID = $(this).parent().parent().parent().attr('scanid');
		ScansList[activeProcessId].ScansContainer[intervalID].max = max;
	});

	/*
	 * Modify field: Scan-inc value
	 */
	$(document).on("change", ".conf-scan-inc", function()	{
		var inc = $(this).val();
		var intervalID = $(this).parent().parent().parent().attr('scanid');
		ScansList[activeProcessId].ScansContainer[intervalID].inc = inc;
	});

	/*
	 * Plot fields modification
	 */
	$("#scan-plot-title").change(function(){
		 ScansList[activeProcessId].title = $("#scan-plot-title").val();
	});

	$("#scan-plot-xlabel").change(function(){
		 ScansList[activeProcessId].xlabel = $("#scan-plot-xlabel").val();
	});

	$("#scan-plot-ylabel").change(function(){
		 ScansList[activeProcessId].ylabel = $("#scan-plot-ylabel").val();
	});

	$("#scan-plot-xmin").change(function(){
		 ScansList[activeProcessId].xmin = $("#scan-plot-xmin").val();
	});

	$("#scan-plot-xmax").change(function(){
		 ScansList[activeProcessId].xmax = $("#scan-plot-xmax").val();
	});
