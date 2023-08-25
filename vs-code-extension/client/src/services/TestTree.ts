import { TextDecoder } from 'util';
import * as vscode from 'vscode';
import { parseMarkdown } from './CobolCheckInputParser';
import internal = require('stream');
import { integer } from 'vscode-languageclient';
import { getConfigurationValueFor } from './CobolCheckConfiguration';
import { appendPath, getCobolCheckRunArgumentsBasedOnCurrentDirectory, getCobolCheckRunArgumentsBasedOnCurrentFile, runCobolCheck } from './CobolCheckLauncher';
import { handleCobolCheckOut, handleCobolCheckOutput } from '../Helpers/ExtensionHelper';

const textDecoder = new TextDecoder('utf-8');
let externalVsCodeInstallationDir = vscode.extensions.getExtension("openmainframeproject.cobol-check-extension").extensionPath;
let configPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/config.properties');
let cobolCheckJarPath = appendPath(externalVsCodeInstallationDir, 'Cobol-check/bin/cobol-check-0.2.8.jar');



export type MarkdownTestData = TestFile | TestHeading | TestCase;

// only for the root files
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

	constructor(path:string) {
		this.setDirectory(path) 
	}
	

	public async updateFromDisk(controller: vscode.TestController, item: vscode.TestItem) {
		try {
			if(!this.getIsDirectory()){
				var content= await getContentFromFilesystem(item.uri!, (item.children.size==0));
				item.error = undefined;
				this.updateFromContents(controller, content, item, item.children.size);
				
			}else{
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
				// console.log("label")
				// console.log(label)
				// console.log("content")
				// console.log(content)
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
		// if(isFile) ascend(1)
		// else ascend(0) // finish and assign children for all remaining items
	}

	public setDirectory(fileName:string){
		if(fileName.includes(".cut")) this.isDirectory=false
		else this.isDirectory=true
	}

	public getIsDirectory(){
		return this.isDirectory;
	}

	async run(item: vscode.TestItem, options: vscode.TestRun): Promise<void> {
		let applicationSourceDir = await getConfigurationValueFor(configPath, 'application.source.directory');
		let argument : string;
		const data = testData.get(item);
		if(data instanceof TestFile && ! data.getIsDirectory()){
			argument = getCobolCheckRunArgumentsBasedOnCurrentFile(externalVsCodeInstallationDir, configPath, applicationSourceDir, item.uri.fsPath );
		}else{
			argument = getCobolCheckRunArgumentsBasedOnCurrentDirectory(externalVsCodeInstallationDir, configPath, applicationSourceDir, item.uri.fsPath );
		}
		const start = Date.now();
		let output = await runCobolCheck(cobolCheckJarPath, argument)
		const result = await handleCobolCheckOut(output,externalVsCodeInstallationDir,configPath);
		const duration = Date.now() - start;
		if(result) options.passed(item, duration);
		else{
			const message = vscode.TestMessage.diff(`Expected ${item.label}`, String("123"), String("223"));
			options.failed(item, message, duration)
		} 
	}

}

export class TestHeading {
	constructor(
		public generation: number,
		private content: string
		) { 
	}
	async run(item: vscode.TestItem, options: vscode.TestRun): Promise<void> {
		// let applicationSourceDir = await getConfigurationValueFor(configPath, 'application.source.directory');
		// let argument : string = getCobolCheckRunArgumentsBasedOnCurrentFile(externalVsCodeInstallationDir, configPath, applicationSourceDir);
		// const output = runCobolCheck()		// let applicationSourceDir = await getConfigurationValueFor(configPath, 'application.source.directory');
		// let argument : string = getCobolCheckRunArgumentsBasedOnCurrentFile(externalVsCodeInstallationDir, configPath, applicationSourceDir, item.uri.fsPath );
		// let output = await runCobolCheck(cobolCheckJarPath, argument)

		const start = Date.now();
		await new Promise(resolve => setTimeout(resolve, 1000 + Math.random() * 1000));
		const duration = Date.now() - start;
		const message = vscode.TestMessage.diff(`Expected ${item.label}`, String("123"), String("This should not run"));
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
		// let applicationSourceDir = await getConfigurationValueFor(configPath, 'application.source.directory');
		// let argument : string = getCobolCheckRunArgumentsBasedOnCurrentFile(externalVsCodeInstallationDir, configPath, applicationSourceDir);
		// const output = runCobolCheck()

		// Paste later
		// const start = Date.now();
		// let applicationSourceDir = await getConfigurationValueFor(configPath, 'application.source.directory');
		// let argument : string = getCobolCheckRunArgumentsBasedOnCurrentFile(externalVsCodeInstallationDir, configPath, applicationSourceDir, item.uri.fsPath);
		
		// // TODO here
		// console.log("argument!!!")
		// console.log(this.label) // test case name 
		// console.log(item.uri.fsPath) // file path
		// console.log("externalVsCodeInstallationDir")
		// console.log(externalVsCodeInstallationDir)
		// console.log("configPath")
		// console.log(configPath)
		// console.log("applicationSourceDir")
		// console.log(applicationSourceDir)
		// console.log("argument")
		// console.log(argument)
		// let output = await runCobolCheck(cobolCheckJarPath, argument)
		// const result = await handleCobolCheckOutput(output);
		// // await new Promise(resolve => setTimeout(resolve, 1000 + Math.random() * 1000));
		// const duration = Date.now() - start;
		// if(result) options.passed(item, duration);
		// else{
		// 	const message = vscode.TestMessage.diff(`Expected ${item.label}`, String("123"), String("223"));
		// 	options.failed(item, message, duration)
		// } 

		const start = Date.now();
		await new Promise(resolve => setTimeout(resolve, 1000 + Math.random() * 1000));
		const duration = Date.now() - start;
		const message = vscode.TestMessage.diff(`Expected ${item.label}`, String("123"), String("This should not run"));
		// options.failed(item, message ,duration);
		options.passed(item, duration);
	}

	
}

