import * as vscode from 'vscode';
import { integer } from 'vscode-languageclient';

let cobolCheckJar_Windows = '@java -jar Cobol-check\\bin\\cobol-check-0.1.0.jar';
let cobolCheckJar_Linux_Mac = 'java -jar Cobol-check/bin/cobol-check-0.1.0.jar $@';


const windowsPlatform = 'Windows';
const macPlatform = 'MacOS';
const linuxPlatform = 'Linux';

let currentPlatform = getOS();

let lastProgramPath : string = null;

export async function runCobolCheck(path : string, commandLineArgs : string) : Promise<string> {
	return new Promise(async resolve => {
		// Getting the right command based on platform
		let executeJarCommand = '';
		if (currentPlatform === windowsPlatform){
			executeJarCommand = '@java -jar "' + path + '"';
		}else if (currentPlatform === macPlatform || currentPlatform === linuxPlatform){
			executeJarCommand = 'java -jar "' + path + '" $@';
		} else{
			vscode.window.showErrorMessage('Only Windows, Mac OS and Linux are supported for running Cobol Check. ' + 
				'Your platform: ' + currentPlatform + ' is not supported');
			resolve(null);
		}

		try{
			var exec = require('child_process').exec;
			//Run Cobol Check jar with arguments
			var child = exec(executeJarCommand + ' ' + commandLineArgs, (error, stdout, stderr) => {
				if(error !== null){
					console.log("Error -> "+error);
					vscode.window.showErrorMessage('Cobol Check ran with ' + error);
				}
				resolve(error ? stderr : stdout)
			});

		} catch (error){
			console.error(error);
			vscode.window.showErrorMessage('Could no launch Cobol Check: ' + error);
			resolve(null);
		}


     });

}

export function getCobolProgramPathForGivenContext() : string{
	let currentFile = vscode.window.activeTextEditor.document.uri.fsPath;
	let workingDirectory = vscode.workspace.getWorkspaceFolder(vscode.window.activeTextEditor.document.uri).uri.fsPath;

	//Cobol file is open
	if (currentFile.toLocaleUpperCase().endsWith('CBL') || currentFile.toLocaleUpperCase().endsWith('COB')){
		lastProgramPath = currentFile;
		return currentFile;
	}
	//.cut file is open
	else if (currentFile.toLocaleUpperCase().endsWith('CUT')){
		var path = require('path');
		const cobolFileExtensions = ['.CBL', '.cbl', '.COB', '.cob'];
		const programName = path.basename(path.dirname(currentFile))
		for (let extension of cobolFileExtensions){
			let programPath = findFile(programName + extension, workingDirectory);
			if (programPath !== null){
				lastProgramPath = programPath;
				return programPath;
			}
		}
		vscode.window.showErrorMessage('Found no cobol program with the name: ' + programName);
	}
	//Other file is open
	else{
		if (lastProgramPath != null){
			return lastProgramPath;
		}
		else{
			vscode.window.showErrorMessage('No context available for which cobol program to run tests for');
		}
	}
	return null;
}

export function getFileName(path : string, includeExtension : boolean) : string{
	let programName : string = "";
	let lastFileSeperatorIndex = path.lastIndexOf(getFileSeperatorForOS(currentPlatform));
	if (lastFileSeperatorIndex !== -1){
		programName = path.substring(lastFileSeperatorIndex + 1);
	}else{
		programName = path;
	}
	if (includeExtension) return programName;

	let fileExtensionIndex = programName.indexOf('.');
	if (fileExtensionIndex !== -1){
		return programName.substring(0, fileExtensionIndex);
	} else{
		return programName;
	}
}

export function getSourceFolderContextPath(path : string, sourceFolderName : string) : string{
	var lastFileSeperatorIndex : integer;
	var lastFolder : string = null;
	while (lastFolder !== sourceFolderName){
		lastFileSeperatorIndex = path.lastIndexOf(getFileSeperatorForOS(currentPlatform));
		if (lastFileSeperatorIndex !== -1){
			lastFolder = path.substring(lastFileSeperatorIndex + 1);
			//Get parent folder
			path = path.substring(0, lastFileSeperatorIndex)
		}
		else{
			vscode.window.showErrorMessage("Found no folder named '" + sourceFolderName + "' in path: " + path + ". If the source folder " +
				"is named something else, configure cobol check for the following properties: [application.source.directory, " +
				"application.copybook.directory, test.suite.directory]");
			return null;
		}
	}
	return path;
}

export function getRootFolder(path : string){
	path = adjustPath(path);
	let fileSeperatorIndex = path.indexOf(getFileSeperatorForOS(currentPlatform));
	if (fileSeperatorIndex !== -1){
		return path.substring(0, fileSeperatorIndex);
	}
	else{
		return path;
	}
}

export function appendPath(path1 : string, path2 : string){
	return adjustPath(path1) + getFileSeperatorForOS(currentPlatform) + adjustPath(path2);
}

function getOS() {
	var platform = process.platform;
	var	macosPlatformTag = 'darwin'
	var	windowsPlatformTag = 'win32'
	var	linuxPlatformTag = 'linux'
  
	if (platform === macosPlatformTag) {
	  return macPlatform;
	} else if (platform === windowsPlatformTag) {
	  return windowsPlatform;
	} else if (platform === linuxPlatformTag) {
	  return linuxPlatform;
	}
	return platform;
  }

  function getFileSeperatorForOS(platform : string){
	  if (platform === windowsPlatform) return '\\';
	  else return '/';
  }

function adjustPath(path : string){
	if (getFileSeperatorForOS(currentPlatform) === '/')
		return path.split('\\').join('/');
	else
		return path.split('/').join('\\');
}

function findFile(name : String, path : string){

	const fs = require('fs')

	const files = fs.readdirSync(path)

	for (let file of files) {
		if (path.endsWith(getFileSeperatorForOS(currentPlatform))){
			file = path + file;
		}
		else{
			file = path + getFileSeperatorForOS(currentPlatform) + file;
		}
		if (fs.lstatSync(file).isDirectory()){
			let returned = findFile(name, file);
			if (returned !== null){
				return returned;
			}
		}
		else{
			let fileName : string = getFileName(adjustPath(file), true);
			if (fileName === name){
				return(file);
			}
		}
	}
	return null;
}