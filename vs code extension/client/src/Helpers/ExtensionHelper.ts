import * as vscode from 'vscode';
import { appendPath, getCurrentProgramName, getTextFromFile } from '../services/CobolCheckLauncher';

import path = require('path');
import * as CobParser from '../services/CobolCheckOutputParser'
import { getFormattedHtmlSpan, getHtmlWarningDiv, makePathsIntoHtmlLinks, parseAsHtmlDocument } from './HtmlHelper';
import { insertTextAfterFirstToken } from './StringHelper';
import { getConfigurationValueFor } from '../services/CobolCheckConfiguration';

let panel : vscode.WebviewPanel = null;

export async function handleCobolCheckOut(output : CobParser.CobolCheckOutputParser, vsCodeInstallDir:string, configPath:string) : Promise<boolean>{
	return new Promise(async resolve => {
		let testResultFile:string = "";
		let htmlResult:string = "";
		switch(output.state){
			case CobParser.OutputState.NoIssues:
				testResultFile = appendPath(vsCodeInstallDir, await getConfigurationValueFor(configPath, 'test.results.file'));
				htmlResult = await getTextFromFile(testResultFile + '.html');
				showWebWiev('Test Results - ' + getCurrentProgramName(), htmlResult)
				resolve(true);
				break;

			case CobParser.OutputState.SyntaxWarnings:
				testResultFile = appendPath(vsCodeInstallDir, await getConfigurationValueFor(configPath, 'test.results.file'));
				htmlResult = await getTextFromFile(testResultFile + '.html');
				let warningSpan:string = getFormattedHtmlSpan(output.outputText.substring(0, output.outputText.length - 2), true)
				let warningHtmlDiv:string = getHtmlWarningDiv(warningSpan);
				htmlResult = insertTextAfterFirstToken(htmlResult, "</table>", warningHtmlDiv);
				showWebWiev('Test Results - ' + getCurrentProgramName(), htmlResult);
				vscode.window.showWarningMessage("Cobol Check ran with warning(s)");
				resolve(true);
				break;

			case CobParser.OutputState.SyntaxError:
				vscode.window.showErrorMessage("Cobol Check stopped due to Syntax error(s) in one or more test suites");
				showWebWiev('Test Results - ' + getCurrentProgramName(), parseAsHtmlDocument(output.outputText, true))
				resolve(false);
				break;

			case CobParser.OutputState.JavaException:
				vscode.window.showErrorMessage("Cobol Check stopped due to Java Exception");
				showWebWiev('Test Results - ' + getCurrentProgramName(), parseAsHtmlDocument(output.outputText, false))
				resolve(false);
				break;

			case CobParser.OutputState.CmdError:
				vscode.window.showErrorMessage("Cobol Check stopped due to jar call script error");
				showWebWiev('Test Results - ' + getCurrentProgramName(), parseAsHtmlDocument(output.outputText, false))
				resolve(false);
				break;

			case CobParser.OutputState.CobolCompilerError:
				vscode.window.showErrorMessage("Cobol Check stopped due to COBOL compile errors");
				showWebWiev('Test Results - ' + getCurrentProgramName(), parseAsHtmlDocument(output.outputText, true))
				resolve(false);
				break;
				
			default:
				vscode.window.showErrorMessage("Uncaught error occured. Please see the log for more info");
				resolve(false);
				break;
		}
	});
}

function showWebWiev(title: string, html : string){
	if (html !== null){
		if (panel === null){
			panel = vscode.window.createWebviewPanel(
				'testResult', // Identifies the type of the webview. Used internally
				'Test Results - ' + getCurrentProgramName(), // Title of the panel displayed to the user
				vscode.ViewColumn.Two, // Editor column to show the new webview panel in.
				{} // Webview options. More on these later.
			);
			panel.onDidDispose(() => {
				panel = null;
			})
		}
		panel.reveal(vscode.ViewColumn.Two);
		panel.title = title;
		panel.webview.html = html;
	}
}