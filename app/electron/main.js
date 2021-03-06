// Modules to control application life and create native browser window
const {app, BrowserWindow, shell} = require('electron')
const path = require('path')
const childProcess = require('child_process');
const { autoUpdater } = require("electron-updater")
const root = require('electron-root-path').rootPath;

const serverPath = path.resolve(path.join(root, './saffire-mixer'));

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow
let server

function createWindow () {
  // Create the browser window.
  mainWindow = new BrowserWindow({
    width: 1400,
    height: 860,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      nodeIntegration: true
    },
    title: "Saffire LE Mixer"
  })

  // and load the index.html of the app.
  mainWindow.loadFile('www/index.html')

  // Open the DevTools.
  // mainWindow.webContents.openDevTools()
  const handleRedirect = (e, url) => {
    if (url !== e.sender.getURL()) {
      e.preventDefault()
      shell.openExternal(url)
    }
  }

  mainWindow.webContents.on('will-navigate', handleRedirect)

  // Emitted when the window is closed.
  mainWindow.on('closed', function () {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null
  })
}

function startServer() {
  server = childProcess.spawn(serverPath, ["server"], {stdio: 'inherit', cwd: root});
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', function() {
  startServer();
  createWindow();
  autoUpdater.checkForUpdatesAndNotify();
});

// Quit when all windows are closed.
app.on('window-all-closed', function () {
  // On macOS it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== 'darwin') app.quit()
})

app.on('activate', function () {
  // On macOS it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) createWindow()
})

app.on('will-quit', function() {
  if(server !== null) server.kill('SIGINT');
});

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.
