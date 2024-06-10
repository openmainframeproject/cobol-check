// The module 'vscode' contains the VS Code extensibility API 
// Import the module and reference it with the alias vscode in your code below

//Include files ved byg/udgivelse?
//Dele cut og Cobol Check extension op?
//Klasser i typescript virker ikke?


import * as vscode from 'vscode';
import { workspace, ExtensionContext, window} from 'vscode';
import { getConfigurationMap, getConfigurationValueFor, resetConfigurations, setConfiguration } from './services/CobolCheckConfiguration';
import { appendPath, getCobolCheckRunArgumentsBasedOnCurrentFile, getCobolProgramPathForGivenContext, getCurrentProgramName, getFileName, getTextFromFile, getRootFolder, getSourceFolderContextPath, runCobolCheck, getIsInsideTestSuiteDirectory1, getOS, getFileSeperatorForOS } from './services/CobolCheckLauncher';

import { startCutLanguageClientServer, stopCutLanguageClientServer } from './services/cutLanguageClientServerSetup';
import { handleCobolCheckOut } from './Helpers/ExtensionHelper';
import path = require('path');
import { getContentFromFilesystem, MarkdownTestData, TestCase, testData, TestFile, TestHeading } from "./services/TestTree";


let externalVsCodeInstallationDir = vscode.extensions.getExtension("openmainframeproject.cobol-check-extension").extensionPath;
let configPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/config.properties');
let defaultConfigPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/default.properties');
let cobolCheckJarPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/bin/cobol-check-0.2.10.jar');
let currentPlatform = getOS();


export async function activate(context: ExtensionContext) { 
	startCutLanguageClientServer(context);

	const ctrl = vscode.tests.createTestController('CobolCheckController', 'Cobol Check');
	context.subscriptions.push(ctrl);

	const fileChangedEmitter = new vscode.EventEmitter<vscode.Uri>();
	
	let runCobolCheck_Cmd = vscode.commands.registerCommand('cobolcheck.run', (pathToExpandedProgram : string) => {
		//Setting loader
		vscode.window.withProgress({location: vscode.ProgressLocation.Notification, cancellable: true, title: 'Cobol Check running:'}, 
		async (progress) => {
			
			progress.report({ message: 'Setting up run arguments' })

			//Getting arguments to run
			let applicationSourceDir = await getConfigurationValueFor(configPath, 'application.source.directory');
			let argument : string = null
			console.log("Cobol-check extention: pathToExpandedProgram=" + pathToExpandedProgram)	
			if(pathToExpandedProgram === undefined) {
				argument = getCobolCheckRunArgumentsBasedOnCurrentFile(externalVsCodeInstallationDir, configPath, applicationSourceDir, vscode.window.activeTextEditor.document.uri.fsPath);
			}
			else {
				argument = getCobolCheckRunArgumentsBasedOnCurrentFile(externalVsCodeInstallationDir, configPath, applicationSourceDir, pathToExpandedProgram);
			}
			console.log("Cobol-check extention: argument=" + argument)
			if (argument === null) return;

			progress.report({ message: 'Running tests' })

			//Running Cobol Check
			let output = await runCobolCheck(cobolCheckJarPath, argument)
			progress.report({ message: 'Handling output' })
			if (output !== null)
				await handleCobolCheckOut(output, externalVsCodeInstallationDir, configPath);
			else
				vscode.window.showErrorMessage("Uncaught error occured. Please see the log for more info");
		});
	});

	let setConfiguration_Cmd = vscode.commands.registerCommand('cobolcheck.configure', (key : string, value : string) => {
		if (key && value){
			setConfiguration(configPath, key, value);
		}
		else{
			getConfigurationMap(configPath, async (configMap) => {
				if (configMap === null) return;
				key = await vscode.window.showQuickPick(Array.from(configMap.keys()), {placeHolder: 'Pick a configuration'});
				const value = await vscode.window.showInputBox({placeHolder: 'New configuration value'})
				if (!key || !value) return;
				setConfiguration(configPath, key, value);
			});
		}
	});

	let resetConfigurations_Cmd = vscode.commands.registerCommand('cobolcheck.reset.configuration', () => {
		resetConfigurations(configPath, defaultConfigPath);
	});

	context.subscriptions.push(runCobolCheck_Cmd);
	context.subscriptions.push(setConfiguration_Cmd);
	context.subscriptions.push(resetConfigurations_Cmd);

	const runHandler = (request: vscode.TestRunRequest2, cancellation: vscode.CancellationToken) => {
		if (!request.continuous) {
			return startTestRun(request);
		}

		const l = fileChangedEmitter.event(async uri => {
			const res = await getOrCreateFile(ctrl, uri)
			return startTestRun(
			
			new vscode.TestRunRequest2(
				[res.file],
				undefined,
				request.profile,
				true
			),
		)});
		cancellation.onCancellationRequested(() => l.dispose());
	};

	const startTestRun = (request: vscode.TestRunRequest) => {
		const queue: { test: vscode.TestItem; data: MarkdownTestData  }[] = [];
		const run = ctrl.createTestRun(request);
		// map of file uris to statements on each line:
		const coveredLines = new Map</* file uri */ string, (vscode.StatementCoverage | undefined)[]>();

		const discoverTests = async (tests: Iterable<vscode.TestItem>) => {
			if(tests){
				for (const test of tests) {
					if (request.exclude?.includes(test)) {
						continue;
					}

					const data = testData.get(test);
					if (data instanceof TestCase 
						|| (data instanceof TestFile && (!data.getIsInsideTestSuiteDirectory() || data.getIsTestSuiteDirectory())  )
						|| (data instanceof TestHeading && test.children.size==0)
						) {
						run.enqueued(test);
						queue.push({ test, data });
					}
					else if (data instanceof TestFile && data.getIsInsideTestSuiteDirectory()){
						await discoverTests(gatherTestItems(test))
					}

					if(!(data instanceof TestFile) || !data.getIsDirectory())
					{
						if (test.uri && !coveredLines.has(test.uri.toString())) {
						try {
							const lines = (await getContentFromFilesystem(test.uri,true))
							if(lines!=""){
								const lineArr = lines.split('\n');
								coveredLines.set(
									test.uri.toString(),
									lineArr.map((lineText, lineNo) =>
										lineText.trim().length ? new vscode.StatementCoverage(0, new vscode.Position(lineNo, 0)) : undefined
									)
								);
							}
							} catch {
								// ignored
							}
						}
					}
				}
			}
		};

		const runTestQueue = async () => {
			for (const { test, data } of queue) {
				run.appendOutput(`Running ${test.id}\r\n`);
				if (run.token.isCancellationRequested) {
					run.skipped(test);
				} else {
					run.started(test);
					await data.run(test, run);
				}
				
				if(!(data instanceof TestFile)){
					const lineNo = test.range!.start.line;
					const fileCoverage = coveredLines.get(test.uri!.toString());
					if (fileCoverage) {
						// fileCoverage[lineNo]!.executionCount++;
						(fileCoverage[lineNo]!.executed as number)++;
					}
				}

				run.appendOutput(`Completed ${test.id}\r\n`);
			}
			run.end();
		};

		run.coverageProvider = {
			provideFileCoverage() {
				const coverage: vscode.FileCoverage[] = [];
				for (const [uri, statements] of coveredLines) {
					coverage.push(
						vscode.FileCoverage.fromDetails(
							vscode.Uri.parse(uri),
							statements.filter((s): s is vscode.StatementCoverage => !!s)
						)
					);
				}

				return coverage;
			},
		};

		discoverTests(request.include ?? gatherTestItemsFromCollection(ctrl.items)).then(runTestQueue);
	};

	ctrl.refreshHandler = async () => {
		await Promise.all(getWorkspaceTestPatterns().map(({ pattern }) => findRootFiles(ctrl, pattern)));
	};

	ctrl.createRunProfile('Run Tests', vscode.TestRunProfileKind.Run, runHandler, true, undefined, true);

	ctrl.resolveHandler = async item => {
		if (!item) {
			context.subscriptions.push(...startWatchingWorkspace(ctrl, fileChangedEmitter));
			return;
		}
		const data = testData.get(item);
		if (data instanceof TestFile) {
			await data.updateFromDisk(ctrl, item);
		}
	};

	async function updateNodeForDocument(e: vscode.TextDocument) {
		if (e.uri.scheme !== 'file') {
			return;
		}
		if (!e.uri.path.endsWith('.cut')) {
			return;
		}

		const { file, data } = await getOrCreateFile(ctrl, e.uri);
		data.updateFromContents(ctrl, e.getText(), file);
	}

	for (const document of vscode.workspace.textDocuments) {
		await updateNodeForDocument(document);
	}

	context.subscriptions.push(
		vscode.workspace.onDidOpenTextDocument(updateNodeForDocument),
		vscode.workspace.onDidChangeTextDocument(e => updateNodeForDocument(e.document)),
	);
}

export function deactivate() {
	stopCutLanguageClientServer();
}


async function createDirectoryItems( controller:vscode.TestController, uri: vscode.Uri){
	// Create TestFile for each directory

	var isInsideTestSuite: boolean = await getIsInsideTestSuiteDirectory1(uri.fsPath)
	const directory = vscode.workspace.asRelativePath(uri.fsPath)
	let splitChar = '/'
	if(!directory.includes(splitChar))
		splitChar = '\\'
	const dirArr = directory.split(splitChar);
	const rootDir = vscode.workspace.getWorkspaceFolder(uri).uri.fsPath;
	const rootUri = rootDir + '/' + dirArr[0]

	let tmpUri = vscode.Uri.file(rootUri);
	var file :vscode.TestItem = null; 
	var data = null

	if(!controller.items.get(rootUri)){ 
		file = controller.createTestItem(rootUri, dirArr[0], tmpUri);
		controller.items.add(file);
		data = new TestFile();
		data.setDirectoryDetails(rootUri)
		file.canResolveChildren = true;
		testData.set(file, data);
		if(isInsideTestSuite) data.setIsInsideTestSuiteDirectory(true)
	}
	else{ 
		file = controller.items.get(rootUri)
		data = testData.get(file)
		if(isInsideTestSuite) data.setIsInsideTestSuiteDirectory(true)
	}
	if(dirArr.length==1) return {file,data};

	var prevFile: vscode.TestItem = file
	var tmpDir = rootUri
	var tmpFile = null
	var tmpData = null

	for(var i =1;i<dirArr.length;i++){
		tmpDir = tmpDir + '/' + dirArr[i]
		const existing = prevFile.children.get(tmpDir);
		
		if(existing){
			const tmpData = testData.get(existing)
			if(tmpData instanceof TestFile && tmpData.getIsTestSuiteDirectory()){
				isInsideTestSuite=false
			}
			prevFile = existing;
		}
		else{
			tmpUri = vscode.Uri.file(tmpDir);
			tmpFile = controller.createTestItem(tmpDir, dirArr[i], tmpUri);
			tmpData = new TestFile();
			await tmpData.setDirectoryDetails(tmpDir)

			tmpFile.canResolveChildren = true;
			testData.set(tmpFile, tmpData);
			// add to existing tree structure
			prevFile.children.add(tmpFile)

			
			if(isInsideTestSuite && tmpData instanceof TestFile){
				tmpData.setIsInsideTestSuiteDirectory(true)
				if(tmpData.getIsTestSuiteDirectory()){
					isInsideTestSuite=false
				}
			}
			prevFile=tmpFile;
		}
	}
	return {tmpFile,tmpData};
}

function getDirectoryItems( controller:vscode.TestController, uri: vscode.Uri){ 
	const directory = vscode.workspace.asRelativePath(uri.fsPath)
	let splitChar = '/';
	if(!directory.includes(splitChar))
		splitChar = '\\';
	const dirArr = directory.split(splitChar);
	const rootDir = vscode.workspace.getWorkspaceFolder(uri).uri.fsPath;
	
	var tmpDir = rootDir + '/' + dirArr[0];

	var existing :vscode.TestItem = controller.items.get(tmpDir);
	if(!existing) return null

	for(var i = 1; i<dirArr.length ;i++){
		tmpDir = tmpDir + '/' + dirArr[i];
		existing = existing.children.get(tmpDir);
		if(!existing) return null
	}
	return existing;
}

async function getOrCreateFile(controller: vscode.TestController, uri: vscode.Uri) {
	const existing = getDirectoryItems(controller,uri);
	if (existing) {
		return { file: existing, data: testData.get(existing) as TestFile };
	}
	const res = await createDirectoryItems(controller,uri);
	return res
}

function gatherTestItems(test: vscode.TestItem) {
	// testHeading
	const data : MarkdownTestData = testData.get(test)
	var items: vscode.TestItem[] = [];
	
	if(data instanceof TestFile && data.getIsInsideTestSuiteDirectory() && ! data.getIsTestSuiteDirectory() ){
		// add child test if it is a directory 
		test.children.forEach(item => items=items.concat(gatherTestItems(item)));
	}
	else if(data instanceof TestFile){
		items.push(test)
	}
	else if(data instanceof TestHeading && test.children.size==0){
		items.push(test)
	}else if(data instanceof TestHeading ){
		test.children.forEach(item => items.push(item));
	}
	else if(data instanceof TestCase){
		items.push(test);
	}
	return items;

}

function gatherTestItemsFromCollection(collection: vscode.TestItemCollection) {
	var items: vscode.TestItem[] = [];
	collection.forEach(item => {
		items = items.concat(gatherTestItems(item))
	});
	return items;
}

function getWorkspaceTestPatterns() {
	if (!vscode.workspace.workspaceFolders) {
		return [];
	}

	return vscode.workspace.workspaceFolders.map(workspaceFolder => ({
		workspaceFolder,
		pattern: new vscode.RelativePattern(workspaceFolder, '**/*.cut'),
	}));
}
 
async function findRootFiles(controller: vscode.TestController, pattern: vscode.GlobPattern) {
	for (const file of await vscode.workspace.findFiles(pattern)) {
		await getOrCreateFile(controller, file);
	}
}

function startWatchingWorkspace(controller: vscode.TestController, fileChangedEmitter: vscode.EventEmitter<vscode.Uri> ) {
	return getWorkspaceTestPatterns().map(({ workspaceFolder, pattern }) => {
		
		const watcher = vscode.workspace.createFileSystemWatcher(pattern);

		watcher.onDidCreate(uri => {
			getOrCreateFile(controller, uri);
			fileChangedEmitter.fire(uri);
		});
		watcher.onDidChange(async uri => {
			const { file, data } = await getOrCreateFile(controller, uri);
			if (data.didResolve) {
				await data.updateFromDisk(controller, file);
			}
			fileChangedEmitter.fire(uri);
		});
		watcher.onDidDelete(uri => controller.items.delete(uri.toString()));

		findRootFiles(controller, pattern);

		return watcher;
	});
}