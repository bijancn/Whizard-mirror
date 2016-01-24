	var ExternalSindarinList = [];

	function SindarinAlias(str, alias) {
		this.name = str;
		this.alias = alias;
		this.toString = SindarinAliasToString;
	}

	function SindarinAliasToString () {
		return "alias " + this.name + " = " + this.alias;
	}

	function SindarinWriteAliases () {
		for (var i=0; i<this.nElements; i++) {
			if (this.list[i] instanceof SindarinAlias ) {
				this.src += this.list[i].toString() + "\n";
				this.elementsUsed.push(i);
			}
		}
	}

	function rebuildAliasList() {
		$("#pop_aliases").empty();
		$("#pop_aliases").append('<div class="row">');
			for(var i=0; i < ExternalSindarinList.length; i++) {
				if (ExternalSindarinList[i] instanceof SindarinAlias) {
					var alias = ExternalSindarinList[i].toString();
					$("#pop_aliases").append('<div class="col-md-10"><a href="javascript:;" class="alias">' + alias + '</a></div><div class="col-md-2"><a href="javascript:;" class="alias-remove" alias-id=' + i + '><span class="glyphicon glyphicon-remove-sign" aria-hidden="true"></span></a></div>');
				}
			$("#pop_aliases").append('</div>');
			}
	}

	/*
	 * Add new alias, useful with examples
	 */
	function AddAlias(name, str) {
		ExternalSindarinList.push (new SindarinAlias(name, str));
		rebuildAliasList();
	}

	function CleanAlias() {
		ExternalSindarinList = [];
		rebuildAliasList();
	}
