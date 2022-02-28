import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	DidChangeConfigurationNotification,
	CompletionItem,
	CompletionItemKind,
	TextDocumentPositionParams,
	TextDocumentSyncKind,
	InitializeResult,
	ConnectionStrategy,
} from 'vscode-languageserver/node';

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

import * as LOGGER from './utils/Logger'

//For hard coded logging info, such as server initialization
const now = new Date();

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
let connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
let documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability: boolean = false;
let hasWorkspaceFolderCapability: boolean = false;
let hasDiagnosticRelatedInformationCapability: boolean = false;

connection.onInitialize((params: InitializeParams) => {
	let capabilities = params.capabilities;

	// Does the client support the `workspace/configuration` request?
	// If not, we fall back using global settings.
	hasConfigurationCapability = !!(
		capabilities.workspace && !!capabilities.workspace.configuration
	);
	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);
	hasDiagnosticRelatedInformationCapability = !!(
		capabilities.textDocument &&
		capabilities.textDocument.publishDiagnostics &&
		capabilities.textDocument.publishDiagnostics.relatedInformation
	);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			// Tell the client that this server supports code completion.
			completionProvider: {
				resolveProvider: true
			}
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			}
		};
	}
	return result;
});

connection.onInitialized(() => {
	//Send information to client log through connection.
	connection.console.log("[Server - " + now.toLocaleTimeString() + " - INFO] Server started.")

	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, undefined);
	}
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			connection.console.log('Workspace folder change event received.');
		});
	}
});

// The example settings
interface ExampleSettings {
	maxNumberOfProblems: number;
	loglevel: string;
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: ExampleSettings = { maxNumberOfProblems: 1000, loglevel: 'ALL' };
let globalSettings: ExampleSettings = defaultSettings;

// Cache the settings of all open documents
let documentSettings: Map<string, Thenable<ExampleSettings>> = new Map();

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) {
		// Reset all cached document settings
		documentSettings.clear();
	} else {
		globalSettings = <ExampleSettings>(
			(change.settings.cut || defaultSettings)
		);
	}

	// Revalidate all open text documents
	documents.all().forEach(validateTextDocument);
});

function getDocumentSettings(resource: string): Thenable<ExampleSettings> {
	if (!hasConfigurationCapability) {
		return Promise.resolve(globalSettings);
	}
	let result = documentSettings.get(resource);
	if (!result) {
		result = connection.workspace.getConfiguration({
			scopeUri: resource,
			section: 'cut'
		});
		documentSettings.set(resource, result);
	}
	return result;
}

// Only keep settings for open documents
documents.onDidClose(e => {
	documentSettings.delete(e.document.uri);
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	validateTextDocument(change.document);
});

// Finds mismatched delimiters.
function pairDelimiters(text: string, openpattern: RegExp, closepattern: RegExp) {
	const pattern = new RegExp(openpattern.source + '|' + closepattern.source, 'gm')
	const matches = [];
	var match: RegExpExecArray | null;

	while (match = pattern.exec(text)) {
		if (openpattern.test(match[0]) || closepattern.test(match[0])) {
			matches.push(match)
		}
	}

	for (let i = 0; i <= matches.length - 1; i++) {
		if (i % 2 == 0) {
			if (!openpattern.test(matches[i][0])) {
				return matches[i]
			}
			if (i == matches.length - 1) {
				return matches[i]
			}
		}
		else {
			if (!closepattern.test(matches[i][0])) {
				return matches[i - 1]
			}
		}
	}
	return null
}

async function initLogger(resource: string): Promise<void> {
	let settings = await getDocumentSettings(resource)
	LOGGER.createLogger(connection.console, settings.loglevel)
	LOGGER.log('Server evaluating: ' + resource, LOGGER.INFO)
}

documents.onDidOpen(open => {
	initLogger(open.document.uri)
});


async function validateTextDocument(textDocument: TextDocument): Promise<void> {
	// We get the settings for every validate run.
	let settings = await getDocumentSettings(textDocument.uri);

	let text = textDocument.getText();
	let m: RegExpExecArray | null;

	// Find unclosed delimiters of each type. 
	//TODO: Make regex exclude keywords inside quotes.
	//TODO: Make regex' more efficient. Somewhat slow on large files.
	const delimiters = 
	[[/(?<!^[0-9]{6}\* *)(?=([^']*'[^']*')*[^']*$)BEFORE EACH/, /(?<!^[0-9]{6}\* *)(?=([^']*'[^']*')*[^']*$)END-BEFORE/],
	[/(?<!^[0-9]{6}\* *)(?=([^']*'[^']*')*[^']*$)(?<!END-)MOCK/, /(?<!^[0-9]{6}\* *)(?=([^']*'[^']*')*[^']*$)END-MOCK/],
	[/(?<!^[0-9]{6}\* *)(?=([^']*'[^']*')*[^']*$)AFTER EACH/, /(?<!^[0-9]{6}\* *)(?=([^']*'[^']*')*[^']*$)END-AFTER/]];
	const delimitererrors = [];
	let error;

	for (let d of delimiters) {
		if (error = pairDelimiters(text, d[0], d[1])) {
			delimitererrors.push(error)
		}
	}

	let problems = 0;
	let diagnostics: Diagnostic[] = [];

	for (let error of delimitererrors) {
		if (problems < settings.maxNumberOfProblems) {
			problems++;
			let diagnostic: Diagnostic = {
				severity: DiagnosticSeverity.Error,
				range: {
					start: textDocument.positionAt(error.index),
					end: textDocument.positionAt(error.index + error[0].length)
				},
				message: `Unclosed ${error[0]}`,
				source: 'cut'
			};
			diagnostics.push(diagnostic);
		}
	}

	// Send the computed diagnostics to VSCode.
	connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

connection.onDidChangeWatchedFiles(_change => {
	// Monitored files have change in VSCode
	connection.console.log('We received a file change event')
});


// This handler provides an empty list of completion items.
connection.onCompletion(
	(_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
		// The pass parameter contains the position of the text document in
		// which code complete got requested. Completion is to be implemented here.
		return [
		];
	}
);

// This handler resolves additional information for the item selected in
// the completion list.
connection.onCompletionResolve(
	(item: CompletionItem): CompletionItem => {
		return item;
	}
);

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
