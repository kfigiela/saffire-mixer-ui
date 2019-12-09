
exports.openInfo = function () {
  var url = "https://github.com/kfigiela/saffire-mixer-ui/blob/master/README.md";
  if(window.nodeRequire) {
    var electron = window.nodeRequire("electron")
    electron.shell.openExternal(url);
  } else {
    window.open(url);
  }
};
