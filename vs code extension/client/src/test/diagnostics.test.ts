import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

suite('Should get diagnostics', () => {
	const docUriEF = getDocUri('errorfree.cut');
	const docUriMD = getDocUri('missingdelimiters.cut');
	const docUriSE = getDocUri('syntaxerror.cut');
	const docUriMT = getDocUri('missingtestsuite.cut')

	test('Reports no error in error free file', async () => {
		//expectedDiagnostics empty since there are no errors
		await testDiagnostics(docUriEF, []);
	});

	test('Reports no error in missing delimiters file', async () => {
        //expectedDiagnostics empty since there are no errors
        await testDiagnostics(docUriMD, []);
    });

	test('Reports general syntax error', async () => {
		await testDiagnostics(docUriSE, [
			{ message: 'Cannot find name CONTINU', range: toRange(7, 11, 7, 18), severity: vscode.DiagnosticSeverity.Error, source: 'cut' },
			{ message: 'Cannot find name MOV', range: toRange(13, 11, 13, 14), severity: vscode.DiagnosticSeverity.Error, source: 'cut' }
		]);
	})

	test('Reports missing TESTSUITE', async () => {
		await testDiagnostics(docUriMT, [
			{ message: 'Missing TESTSUITE statement', range: toRange(10, 11, 10, 12), severity: vscode.DiagnosticSeverity.Error, source: 'cut' }
		]);
	})
});

function toRange(sLine: number, sChar: number, eLine: number, eChar: number) {
	const start = new vscode.Position(sLine, sChar);
	const end = new vscode.Position(eLine, eChar);
	return new vscode.Range(start, end);
}

async function testDiagnostics(docUri: vscode.Uri, expectedDiagnostics: vscode.Diagnostic[]) {
	await activate(docUri);

	const actualDiagnostics = vscode.languages.getDiagnostics(docUri);

	assert.strictEqual(actualDiagnostics.length, expectedDiagnostics.length);

	expectedDiagnostics.forEach((expectedDiagnostic, i) => {
		const actualDiagnostic = actualDiagnostics[i];
		assert.strictEqual(actualDiagnostic.message, expectedDiagnostic.message);
		assert.deepStrictEqual(actualDiagnostic.range, expectedDiagnostic.range);
		assert.strictEqual(actualDiagnostic.severity, expectedDiagnostic.severity);
	});
}