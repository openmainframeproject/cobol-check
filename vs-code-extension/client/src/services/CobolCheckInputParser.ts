import * as vscode from 'vscode';

const testCaseRegex = /^\s*(TESTCASE)\s*(.*)/i;
const testSuiteRegex = /^\s*(TESTSUITE)\s*(.*)/i;

export const parseMarkdown = (text: string, events: {
	onTest(range: vscode.Range, name: string): void;
	onHeading(range: vscode.Range, name: string, depth: number): void;
}) => {

	if(text!=null){
		
		const lines = text.split('\n');
		for (let lineNo = 0; lineNo < lines.length; lineNo++) {
			const line = lines[lineNo];

			const testCase = testCaseRegex.exec(line);
			if (testCase) {
				var [,pounds, name] = testCase;
				name = name.replace(/['"]+/g, '');
				const range = new vscode.Range(new vscode.Position(lineNo, 0), new vscode.Position(lineNo, testCase[0].length));
				events.onTest(range, name);
				continue;
			}
			
			
			const testSuite = testSuiteRegex.exec(line);
			if (testSuite) {
				var [,pounds, name] = testSuite;
				name = name.replace(/['"]+/g, '');
				const range = new vscode.Range(new vscode.Position(lineNo,  line.indexOf(pounds)), new vscode.Position(lineNo,  line.indexOf(name) + name.length));
				events.onHeading(range, name, 1);
			}
		}
	}
};
