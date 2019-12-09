var electron = window.nodeRequire("electron")

exports.openInfo = function () {
  electron.shell.openExternal("https://github.com/kfigiela/saffire-mixer-ui/blob/master/README.md");
};
