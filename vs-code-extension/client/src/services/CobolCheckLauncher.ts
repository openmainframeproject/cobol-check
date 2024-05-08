import * as vscode from 'vscode';
import { integer } from 'vscode-languageclient';
import * as LOGGER from '../utils/Logger'
import * as CobParser from './CobolCheckOutputParser'
import { getConfigurationValueFor } from './CobolCheckConfiguration';
import { subProcess, subProcessSync } from 'subspawn';



const windowsPlatform = 'Windows';
const macPlatform = 'MacOS';
const linuxPlatform = 'Linux';

let currentPlatform = getOS();

let currentProgramName : string = null;

let externalVsCodeInstallationDir = vscode.extensions.getExtension("openmainframeproject.cobol-check-extension").extensionPath;
let configPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/config.properties');


export function getCobolCheckRunArgumentsBasedOnCurrentFile(vsCodeInstallPath : string, configPath : string, sourceDir : string, filePath:string) : string {
	// let currentFile = vscode.window.activeTextEditor.document.uri.fsPath;
	
	//Getting the source path
	const srcFolderName = getRootFolder(sourceDir);
	const srcFolderContext : string = getSourceFolderContextPath(filePath, srcFolderName);
	const cobolSourcePath = appendPath(srcFolderContext, sourceDir);
	LOGGER.log("Found source folder path: " + cobolSourcePath, LOGGER.INFO)
	let programPath : string = getCobolProgramPathForGivenContext(filePath, cobolSourcePath);
	if (programPath === null) return null;
	let programName : string = getFileName(programPath, false);
	currentProgramName = programName;
	LOGGER.log("Found source program name: " + programName, LOGGER.INFO)
	let cutName = getCutName(filePath);
	return '-p ' + programName + ' -t '+ cutName + ' -c "' + configPath + '" -s "' + srcFolderContext + '" ' +
	'-r "' + vsCodeInstallPath + '"'; 
	
}

export function getCobolCheckRunArgumentsBasedOnCurrentDirectory(vsCodeInstallPath : string, configPath : string, sourceDir : string, filePath:string) : string {

	const srcFolderName = getRootFolder(sourceDir);
	const srcFolderContext : string = getSourceFolderContextPath(filePath, srcFolderName);

	if (srcFolderContext === null) return null;
	const cobolSourcePath = appendPath(srcFolderContext, sourceDir);
	LOGGER.log("Found source folder path: " + cobolSourcePath, LOGGER.INFO)
	//Getting program name based on current context
	let programName : string = getFileName(filePath, false);
	currentProgramName = programName;
	LOGGER.log("Found source directory name: " + programName, LOGGER.INFO)
	return '-p ' + programName + ' -c "' + configPath + '" -s "' + srcFolderContext + '" ' +
	'-r "' + vsCodeInstallPath + '"';
}

export function getCobolCheckRunArgumentsBasedOnSuiteDirectory(vsCodeInstallPath : string, configPath : string, sourceDir : string, testFilesPaths:string[]) : string {

	
	const srcFolderName = getRootFolder(sourceDir);
	const srcFolderContext : string = getSourceFolderContextPath(testFilesPaths[0], srcFolderName);
	if (srcFolderContext === null) return null;
	const cobolSourcePath = appendPath(srcFolderContext, sourceDir);

	LOGGER.log("Found source folder path: " + cobolSourcePath, LOGGER.INFO)
	//Getting program name based on current context
	
	let programName: string = testFilesPaths.map(item => getFileName(item, false)).join(' ');
	currentProgramName = programName;

	LOGGER.log("Found source directory name: " + programName, LOGGER.INFO)
	return '-p ' + programName + ' -c "' + configPath + '" -s "' + srcFolderContext + '" ' +
	'-r "' + vsCodeInstallPath + '"';
}

export async function runCobolCheck(path : string, commandLineArgs : string) : Promise<CobParser.CobolCheckOutputParser> {
	return new Promise(async (resolve, reject) => {
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

		LOGGER.log("Running Cobol Check with arguments: " + commandLineArgs, LOGGER.INFO)
		try{
			var killProcess = require('kill-process-by-name');
			let maxWaitMillisec = vscode.workspace.getConfiguration("cut").maxWaitTime;
			LOGGER.log("MAX WAIT TIME SET TO: " + maxWaitMillisec, LOGGER.INFO);
			var exec = require('child_process').exec;
			const testPath = appendPath(externalVsCodeInstallationDir, "Cobol-check");
			// fix for MacOS
			const tmpStr = "cd " + testPath + " && "
				
			//Run Cobol Check jar with arguments
			var child = exec(tmpStr + executeJarCommand + ' ' + commandLineArgs, {timeout:maxWaitMillisec}, (error : string, stdout : string, stderr : string) => {
				if(error !== null){
					LOGGER.log("*** COBOL CHECK ERROR: " + error, LOGGER.ERROR);
				}
				killProcess('cobc');
				LOGGER.log("child.pid: " + child.pid, LOGGER.INFO);
				LOGGER.log("Cobol Check out:\n" + stdout, LOGGER.INFO)
				LOGGER.log("Cobol Check log:\n" + stderr, LOGGER.INFO)
				resolve(new CobParser.CobolCheckOutputParser(stdout, stderr, error))
			});

		} catch (error){
			console.error(error);
			vscode.window.showErrorMessage('Could not launch Cobol Check: ' + error);
			LOGGER.log("*** COBOL CHECK LAUNCHING ERROR: " + error, LOGGER.ERROR);
			resolve(null);
		}
     });
}

export function getCurrentProgramName() : string{
	return currentProgramName;
}

export function getTextFromFile(path : string) : Promise<string>{
	const fs = require('fs');

	return new Promise(async resolve => {

		fs.readFile(path, 'utf8', function(err, data) {
			if(err){
				vscode.window.showErrorMessage("Got an error while trying to read from file: " + path + "\n " + err);
				resolve(null);
			}

			resolve(data.toString());
		});

	 });
}

export function getCobolProgramPathForGivenContext(currentFile : string, cobolSourcePath : string) : string{

	//Cobol file is open
	if (currentFile.toLocaleUpperCase().endsWith('CBL') || currentFile.toLocaleUpperCase().endsWith('COB')){
		return currentFile;
	}
	//.cut file is open
	else if (currentFile.toLocaleUpperCase().endsWith('CUT')){
		var path = require('path');
		const cobolFileExtensions : string[] = ['.CBL', '.cbl', '.COB', '.cob'];
		const programName = path.basename(path.dirname(currentFile))

		let programPath = findFile(programName, cobolFileExtensions, cobolSourcePath);
		if (programPath !== null){
			return programPath;
		}
		vscode.window.showErrorMessage('Found no cobol program with the name: ' + programName + ' in ' + cobolSourcePath);
	}
	//Other file is open
	else{
		vscode.window.showErrorMessage('No context available for which cobol program to run tests for');
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

export function getCutName(path : string) : string{

	// const name = path.split(getFileSeperatorForOS(currentPlatform)).pop()
	let splitChar = '/';
	if(!path.includes(splitChar))
		splitChar = '\\'
	const name = path.split(splitChar).pop()
	if(!name.endsWith(".cut")) return null
	return name;
}

export function getFileExtension(path : string) : string{
	let programName : string = getFileName(path, true);
	let fileExtensionIndex = programName.indexOf('.');
	if (fileExtensionIndex !== -1){
		return programName.substring(fileExtensionIndex);
	} else{
		return "";
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

export function getOS() {
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

export async function getIsInsideTestSuiteDirectory1(input: string): Promise<boolean>{
	let testSuiteDir = await getConfigurationValueFor(configPath, 'test.suite.directory');
	if(input.includes(testSuiteDir)) return true
	else return false
}

export function getFileSeperatorForOS(platform : string){
	  if (platform === windowsPlatform) return '\\';
	  else return '/';
  }

function adjustPath(path : string){
	if (getFileSeperatorForOS(currentPlatform) === '/')
		return path.split('\\').join('/');
	else
		return path.split('/').join('\\');
}

function findFile(name : String, extensions : string[], path : string){

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
			let returned = findFile(name, extensions, file);
			if (returned !== null){
				return returned;
			}
		}
		else{
			let fileName : string = getFileName(adjustPath(file), false);
			if (fileName === name){
				let fileExtension = getFileExtension(adjustPath(file));
				for (let extension of extensions){
					if (fileExtension === extension)
						return(file);
				}
			}
		}
	}
	return null;
}
