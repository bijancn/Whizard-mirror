// TODO: (bcn 2016-04-01) why is it called externalsindarinlist?
export let ExternalSindarinList = [];

function SindarinAliasToString() {
  return 'alias ' + this.name + ' = ' + this.alias;
}

export function SindarinAlias(str, alias) {
  this.name = str;
  this.alias = alias;
  this.toString = SindarinAliasToString;
}

/* TODO: (bcn 2016-03-27) it is very unclear what *this* will be for this function */
// it is not even testable at this level
export function SindarinWriteAliases() {
  for (let i = 0; i < this.nElements; i++) {
    if (this.list[i] instanceof SindarinAlias) {
      this.src += this.list[i].toString() + '\n';
      this.elementsUsed.push(i);
    }
  }
}

export function rebuildAliasList() {
  $('#pop_aliases').empty();
  $('#pop_aliases').append('<div class="row">');
  for (let i = 0; i < ExternalSindarinList.length; i++) {
    if (ExternalSindarinList[i] instanceof SindarinAlias) {
      const alias = ExternalSindarinList[i].toString();
      $('#pop_aliases').append('<div class="col-md-10">' +
          '<a href="javascript:;" class="alias">' + alias + '</a></div>' +
          '<div class="col-md-2"><a href="javascript:;" class="alias-remove" alias-id='
          + i + '><span class="glyphicon glyphicon-remove-sign" ' +
          'aria-hidden="true"></span></a></div>');
    }
    $('#pop_aliases').append('</div>');
  }
}

// TODO: (bcn 2016-04-16) why is this capitalized? does it build a class?
// Add new alias, useful with examples
export function AddAlias(name, str) {
  ExternalSindarinList.push(new SindarinAlias(name, str));
  rebuildAliasList();
}

export function cleanAlias() {
  ExternalSindarinList = [];
  rebuildAliasList();
}
