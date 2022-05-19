// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below

//Include files ved byg/udgivelse?
//Dele cut og Cobol Check extension op?
//Klasser i typescript virker ikke?


import * as vscode from 'vscode';
import { workspace, ExtensionContext, window} from 'vscode';
import { getConfigurationMap, getConfigurationValueFor, resetConfigurations, setConfiguration } from './services/CobolCheckConfiguration';
import { getCobolProgramPathForGivenContext, getFileName, getRootFolder, getSourceFolderContextPath, runCobolCheck } from './services/CobolCheckLauncher';

import { startCutLanguageClientServer, stopCutLanguageClientServer } from './services/cutLanguageClientServerSetup';
import { ResultWebView } from './services/ResultWebView';

let configPath = 'Cobol-check/config.properties';
let defaultConfigPath = 'Cobol-check/default.properties';

let lastCurrentFile = null;
let cutLanguageRunning = false;

export function activate(context: ExtensionContext) {
	startCutLanguageClientServer(context);

	const provider = new ResultWebView(context.extensionUri);

	context.subscriptions.push(
		vscode.window.registerWebviewViewProvider(ResultWebView.viewType, provider));

	let runCobolCheck_Cmd = vscode.commands.registerCommand('cobolcheck.run', () => {
		//Setting loader
		vscode.window.withProgress({location: vscode.ProgressLocation.Window, cancellable: false, title: 'Running tests'}, 
		async (progress) => {
			progress.report({  increment: 0 });

			//Getting program name based on current context
			let programPath : string = getCobolProgramPathForGivenContext();
			if (programPath === null) return;
			let programName : string = getFileName(programPath, false);
			//Getting the name of the source folder, to get the source path
			let applicationSourceDir = await getConfigurationValueFor(configPath, 'application.source.directory');
			let srcFolderContext : string = getSourceFolderContextPath(programPath, getRootFolder(applicationSourceDir));
			if (srcFolderContext === null) return;
			let argument : string = '-p ' + programName + ' -c ' + configPath + ' -s "' + srcFolderContext + '"'
			//Running Cobol Check
			let output = await runCobolCheck(argument)
			if (output !== null)
				provider.showTestResult(output);
			else
				provider.showTestResult("Could not get any test results")
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


