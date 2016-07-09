export let aliasList = [];


export function SindarinAlias(str, alias) {
  this.name = str;
  this.alias = alias;
  this.writeToSindarin = () => 'alias ' + this.name + ' = ' + this.alias;
}


function rebuildAliasList() {
  $('#pop_aliases').empty();
  $('#pop_aliases').append('<div class="row">');
  for (let i = 0; i < aliasList.length; i++) {
    const alias = aliasList[i].writeToSindarin();
    $('#pop_aliases').append('<div class="col-md-10">' +
        '<a href="javascript:;" class="alias">' + alias + '</a></div>' +
        '<div class="col-md-2"><a href="javascript:;" class="alias-remove" alias-id='
        + i + '><span class="glyphicon glyphicon-remove-sign" ' +
        'aria-hidden="true"></span></a></div>');
    $('#pop_aliases').append('</div>');
  }
}


export function addAlias(name, str) {
  aliasList.push(new SindarinAlias(name, str));
  rebuildAliasList();
}


export function removeAlias(id) {
  aliasList.splice(id, 1);
  rebuildAliasList();
}


export function cleanAlias() {
  aliasList = [];
  rebuildAliasList();
}
