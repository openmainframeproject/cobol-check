'use strict';

var childProcess = require('child_process');
var exec = childProcess.exec;

module.exports = function (programname) {
        switch(process.platform) {
                case 'win32':
                 exec('taskkill /F /IM ' + programname + '.exe /T');
                 break;
                default: //Linux + Darwin
                 exec('pkill -f ' + programname);
                 break;
        }
}