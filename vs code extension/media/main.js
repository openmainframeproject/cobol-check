//@ts-check

// This script will be run within the webview itself
// It cannot access the main VS Code APIs directly.
(function () {
    const vscode = acquireVsCodeApi();

    document.getElementById("cobolcheck-run-button").addEventListener('click', () => {
        vscode.postMessage({ type: 'command', value: 'cobolcheck.run' });
    });

    // Handle messages sent from the extension to the webview
    window.addEventListener('message', event => {
        const message = event.data; // The json data that the extension sent
        switch (message.type) {
            case 'test-result':
                {
					document.getElementById('test-result-paragraph').textContent = message.value;
                    break;
                }
        }
    });
}());