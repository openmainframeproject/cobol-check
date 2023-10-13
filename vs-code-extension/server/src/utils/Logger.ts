import * as server from 'vscode-languageserver'

let connectionConsole:server.RemoteConsole;

export function createLogger(_connectionConsole:server.RemoteConsole, loglevel:string){
    connectionConsole = _connectionConsole
}

export function log(inp:string, loglevel:string){
    
    const curLogLevel = loglevel

    if(isLoggingEnabled(curLogLevel, loglevel)){
        const now = new Date();
        connectionConsole.log("[Server - " + now.toLocaleTimeString() + " - " + loglevel + "] " + inp)
    }
}

function isLoggingEnabled(current:string, element:string):boolean {
    let isLoggingEnabledBool = false;

    switch (element) {
        case ALL: 
            isLoggingEnabledBool = isAll(current); 
            break;
        case DEBUG:
            isLoggingEnabledBool = isDebug(current);
            break;
        case INFO:
            isLoggingEnabledBool = isInfo(current);
            break; 
        case WARN:
            isLoggingEnabledBool = isWarn(current);
            break;
        case ERROR:
            isLoggingEnabledBool = isError(current);
            break;
        case FATAL:
            isLoggingEnabledBool = isFatal(current);
            break;
        default:
            break;
    }

    return isLoggingEnabledBool;
}

function isAll(level:string):boolean{
    return level === ALL;
}

function isDebug(level:string):boolean{
    return isAll(level) || level === DEBUG;
}

function isInfo(level:string):boolean {
    return isDebug(level) || level === INFO;
}

function isWarn(level:string):boolean {
    return isInfo(level) || level === WARN;
}

function isError(level:string):boolean {
    return isWarn(level) || level === ERROR;
}

function isFatal(level:string):boolean {
    return isError(level) || level === FATAL;
}

function isOff(level:string):boolean {
    return level === OFF;
}


export const ALL = "ALL";
export const INFO = "INFO";
export const DEBUG = "DEBUG";
export const WARN = "WARN";
export const ERROR = "ERROR";
export const FATAL = "FATAL";
export const OFF = "OFF";