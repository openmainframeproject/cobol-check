// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below

//Include files ved byg/udgivelse?
//Dele cut og Cobol Check extension op?
//Klasser i typescript virker ikke?


import * as vscode from 'vscode';
import { workspace, ExtensionContext, window} from 'vscode';
import { getConfigurationMap, getConfigurationValueFor, resetConfigurations, setConfiguration } from './services/CobolCheckConfiguration';
import { appendPath, getCobolCheckRunArgumentsBasedOnCurrentFile, getCobolProgramPathForGivenContext, getCurrentProgramName, getFileName, getResultOutput, getRootFolder, getSourceFolderContextPath, runCobolCheck } from './services/CobolCheckLauncher';

import { startCutLanguageClientServer, stopCutLanguageClientServer } from './services/cutLanguageClientServerSetup';
import { ResultWebView } from './services/ResultWebView';

let externalVsCodeInstallationDir = vscode.extensions.getExtension("openmainframeproject.cobol-check-extension").extensionPath;
let configPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/config.properties');
let defaultConfigPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/default.properties');
let cobolCheckJarPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/bin/cobol-check-0.1.0.jar');

let lastCurrentFile = null;
let cutLanguageRunning = false;

export function activate(context: ExtensionContext) {
	startCutLanguageClientServer(context);

	const provider = new ResultWebView(context.extensionUri);
	let panel : vscode.WebviewPanel = null;

	context.subscriptions.push(
		vscode.window.registerWebviewViewProvider(ResultWebView.viewType, provider));

	let runCobolCheck_Cmd = vscode.commands.registerCommand('cobolcheck.run', () => {
		//Setting loader
		vscode.window.withProgress({location: vscode.ProgressLocation.Window, cancellable: true, title: 'Running tests'}, 
		async (progress) => {
			progress.report({  increment: 0 });
			//Getting arguments to run
			let applicationSourceDir = await getConfigurationValueFor(configPath, 'application.source.directory');
			let argument : string = getCobolCheckRunArgumentsBasedOnCurrentFile(externalVsCodeInstallationDir, configPath, applicationSourceDir);
			if (argument === null) return;

			//Running Cobol Check
			let output = await runCobolCheck(cobolCheckJarPath, argument)
			console.log(output);

			let testResultFile = appendPath(externalVsCodeInstallationDir, await getConfigurationValueFor(configPath, 'test.results.file'));
			let htmlResult = await getResultOutput(testResultFile + '.html');
			if (htmlResult !== null){
				if (panel === null){
					panel = vscode.window.createWebviewPanel(
						'testResult', // Identifies the type of the webview. Used internally
						'Test Results - ' + getCurrentProgramName, // Title of the panel displayed to the user
						vscode.ViewColumn.Two, // Editor column to show the new webview panel in.
						{} // Webview options. More on these later.
					);
					panel.onDidDispose(() => {
						panel = null;
					})
				}
				panel.reveal(vscode.ViewColumn.Two);
				panel.title = 'Test Results - ' + getCurrentProgramName;
				panel.webview.html = htmlResult;
			}
			progress.report({ increment: 100 });
		});
	});

	let setConfiguration_Cmd = vscode.commands.registerCommand('cobolcheck.configure', () => {
		getConfigurationMap(configPath, async (configMap) => {
			if (configMap === null) return;
			const configKey = await vscode.window.showQuickPick(Array.from(configMap.keys()), {placeHolder: 'Pick a configuration'});
			if (configKey === undefined) return;

			const newValue = await vscode.window.showInputBox({placeHolder: 'New configuration value'})
			if (newValue === undefined) return;

			setConfiguration(configPath, configKey, newValue);
		});
	});

	let resetConfigurations_Cmd = vscode.commands.registerCommand('cobolcheck.reset.configuration', () => {
		resetConfigurations(configPath, defaultConfigPath);
	});

	context.subscriptions.push(runCobolCheck_Cmd);
	context.subscriptions.push(setConfiguration_Cmd);
	context.subscriptions.push(resetConfigurations_Cmd);
}

export function deactivate() {
	stopCutLanguageClientServer();
}


