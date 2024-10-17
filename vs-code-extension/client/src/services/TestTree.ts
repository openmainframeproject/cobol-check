import { TextDecoder } from 'util';
import * as vscode from 'vscode';
import { parseMarkdown } from './CobolCheckInputParser';
import internal = require('stream');
import { integer } from 'vscode-languageclient';
import { getConfigurationValueFor } from './CobolCheckConfiguration';
import { appendPath, getCobolCheckRunArgumentsBasedOnCurrentDirectory, getCobolCheckRunArgumentsBasedOnCurrentFile, getCobolCheckRunArgumentsBasedOnSuiteDirectory,  runCobolCheck } from './CobolCheckLauncher';
import { handleCobolCheckOut } from '../Helpers/ExtensionHelper';

const textDecoder = new TextDecoder('utf-8');
let externalVsCodeInstallationDir = vscode.extensions.getExtension("openmainframeproject.cobol-check-extension").extensionPath;
let configPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/config.properties');
let cobolCheckJarPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/bin/cobol-check-0.2.12.jar');



export type MarkdownTestData = TestFile | TestHeading | TestCase;

export const testData = new WeakMap<vscode.TestItem, MarkdownTestData>();

let generationCounter = 0;

export const getContentFromFilesystem = async (uri: vscode.Uri, isFile) => {
	try {
		// vscode.workspace.fs.readDirectory
		if(isFile){
			const rawContent = await vscode.workspace.fs.readFile(uri);
			return textDecoder.decode(rawContent);
		}
		return null
		
	} catch (e) {
		console.warn(`Error providing tests for ${uri.fsPath}`, e);
		return '';
	}
};

export class TestFile {

	public didResolve = false;
	private isDirectory:boolean;
	private isInsideTestSuiteDirectory:boolean;
	private isTestSuiteDirectory:boolean;

	constructor() {
	}
	

	public async updateFromDisk(controller: vscode.TestController, item: vscode.TestItem) {
		try {
			if(!this.getIsDirectory()){
				var content= await getContentFromFilesystem(item.uri!, (item.children.size==0));
				item.error = undefined;
				this.updateFromContents(controller, content, item, item.children.size);
				
			}
			else {
				for (const nestedItem of item.children) {
					const data =  testData.get(nestedItem[1]); 
					if(data instanceof TestFile){
						await this.updateFromDisk(controller,nestedItem[1] )
					}
				}
			}
		} catch (e) {
			item.error = (e as Error).stack;
		}
	}

	/**
	 * Parses the tests from the input text, and updates the tests contained
	 * by this file to be those from the text,
	 */
	public updateFromContents(controller: vscode.TestController, content: string, item: vscode.TestItem, fileDepth: integer) {
		const ancestors = [{ item, children: [] as vscode.TestItem[] }];
		const thisGeneration = generationCounter++;
		this.didResolve = true;

		const ascend = (depth: number) => {
			while (ancestors.length > depth) {
				const finished = ancestors.pop()!;
				finished.item.children.replace(finished.children);
			}
		};
		
		parseMarkdown(content, {
			onTest: (range, label) => {
				const parent = ancestors[ancestors.length - 1];
				const data = new TestCase(label,content);
				const id = `${item.uri}/${data.getLabel()}`;
				const tcase = controller.createTestItem(id, data.getLabel(), item.uri);
				testData.set(tcase, data);
				tcase.range = range;
				parent.children.push(tcase);
			},

			onHeading: (range, name, depth) => {
				ascend(depth);
				const parent = ancestors[ancestors.length - 1];
				const id = `${item.uri}/${name}`;
				const thead = controller.createTestItem(id, name, item.uri);
				thead.range = range;
				testData.set(thead, new TestHeading(thisGeneration,content));
				parent.children.push(thead);
				ancestors.push({ item: thead, children: [] });
			},
		});

		ascend(fileDepth)
	}

	public async setDirectoryDetails(fileName:string){
		this.isTestSuiteDirectory=false
		this.isDirectory=true
		this.isInsideTestSuiteDirectory=false
		// if the file is a directory
		if(fileName.endsWith(".cut")) this.isDirectory=false
		// if the file is source directory
		let testSuiteDir = await getConfigurationValueFor(configPath, 'test.suite.directory');
		if(fileName.endsWith(testSuiteDir) ) {
			this.isTestSuiteDirectory=true
		}
	}

	public getIsDirectory(){
		return this.isDirectory;
	}

	public getIsInsideTestSuiteDirectory(){
		// if it is inside test suite directory e.g. src/test is inside src/test/cobol
		return this.isInsideTestSuiteDirectory;
	}

	public getIsTestSuiteDirectory(){
		// If it is test suite directory
		return this.isTestSuiteDirectory;
	}

	public setIsInsideTestSuiteDirectory(value: boolean){
		this.isInsideTestSuiteDirectory = value;
	}


	async run(item: vscode.TestItem, options: vscode.TestRun): Promise<void> {

		const start = Date.now();
		
		let applicationSourceDir = await getConfigurationValueFor(configPath, 'application.source.directory');
		let argument : string;
		const data = testData.get(item);
		if(data instanceof TestFile && ! data.getIsDirectory()){
			// run only one test file
			argument = getCobolCheckRunArgumentsBasedOnCurrentFile(externalVsCodeInstallationDir, configPath, applicationSourceDir, item.uri.fsPath );
		}
		else{
			if( data instanceof TestFile && data.getIsInsideTestSuiteDirectory()){
				// run all test cases inside a program
				const testFilesPaths = this.getFolderNamesInsideTestSuiteDirectory(item)
				argument = getCobolCheckRunArgumentsBasedOnSuiteDirectory(externalVsCodeInstallationDir, configPath, applicationSourceDir, testFilesPaths );
			}else{
				// run all test cases inside default source directory
				argument = getCobolCheckRunArgumentsBasedOnCurrentDirectory(externalVsCodeInstallationDir, configPath, applicationSourceDir, item.uri.fsPath );
			}
		}
		
		let output = await runCobolCheck(cobolCheckJarPath, argument)
		const result = await handleCobolCheckOut(output,externalVsCodeInstallationDir,configPath);
		const duration = Date.now() - start;

		if(result) options.passed(item, duration);
		else{
			// TODO: identify message  
			const message = vscode.TestMessage.diff(`Expected ${item.label}`, String(""), String("0"));
			options.failed(item, message, duration)
		} 
	}

	getFolderNamesInsideTestSuiteDirectory(item: vscode.TestItem): string [] {
		var paths = []
		item.children.forEach(child=>paths.push(child.uri.fsPath))
		return paths
	}

}

export class TestHeading {
	constructor(
		public generation: number,
		private content: string
		) {}
		
	async run(item: vscode.TestItem, options: vscode.TestRun): Promise<void> {
		const start = Date.now();
		await new Promise(resolve => setTimeout(resolve, 1000 + Math.random() * 1000));
		const duration = Date.now() - start;
		// const message = vscode.TestMessage.diff(`Expected ${item.label}`, String(""), String("This should not run"));
		// options.failed(item, message ,duration);
		options.passed(item, duration);
	}
}

export class TestCase {
	constructor(
		private readonly label: string,
		private content: string
	) { }

	getLabel() {
		return this.label;
	}

	async run(item: vscode.TestItem, options: vscode.TestRun): Promise<void> {
		const start = Date.now();
		await new Promise(resolve => setTimeout(resolve, 1000 + Math.random() * 1000));
		const duration = Date.now() - start;
		// const message = vscode.TestMessage.diff(`Expected ${item.label}`, String(""), String("This should not run"));
		// options.failed(item, message ,duration);
		options.passed(item, duration);
	}

	
}

