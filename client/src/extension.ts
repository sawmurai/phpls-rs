import * as path from 'path';
import { workspace, ExtensionContext, IndentAction, languages, window, Uri, TextDocument } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
	Executable
} from 'vscode-languageclient';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	// Used with gratidude from Ben Robert Mewburn
	// https://github.com/bmewburn/vscode-intelephense/blob/master/src/extension.ts
	languages.setLanguageConfiguration('php', {
		wordPattern: /(-?\d*\.\d\w*)|([^\-\`\~\!\@\#\%\^\&\*\(\)\=\+\[\{\]\}\|\;\:\'\"\,\.\<\>\/\?\s\\]+)/g,
		onEnterRules: [
			{
				// e.g. /** | */
				beforeText: /^\s*\/\*\*(?!\/)([^\*]|\*(?!\/))*$/,
				afterText: /^\s*\*\/$/,
				action: { indentAction: IndentAction.IndentOutdent, appendText: ' * ' }
			},
			{
				// e.g. /** ...|
				beforeText: /^\s*\/\*\*(?!\/)([^\*]|\*(?!\/))*$/,
				action: { indentAction: IndentAction.None, appendText: ' * ' }
			},
			{
				// e.g.  * ...|
				beforeText: /^(\t|(\ \ ))*\ \*(\ ([^\*]|\*(?!\/))*)?$/,
				action: { indentAction: IndentAction.None, appendText: '* ' }
			},
			{
				// e.g.  */|
				beforeText: /^(\t|(\ \ ))*\ \*\/\s*$/,
				action: { indentAction: IndentAction.None, removeText: 1 }
			},
			{
				// e.g.  *-----*/|
				beforeText: /^(\t|(\ \ ))*\ \*[^/]*\*\/\s*$/,
				action: { indentAction: IndentAction.None, removeText: 1 }
			}
		]
	});

	let serverModule = context.asAbsolutePath(
		path.join('target', 'debug', 'phpls-rs')
	);

	const run: Executable = {
		command: serverModule,
		options: { cwd: "." },
		args: [
			'--stubs',
			workspace.getConfiguration('phplsrs').get('stubs')
		]
	};

	let serverOptions: ServerOptions = {
		run,
		debug: run
	};

	let clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'php' }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher('**/*.php')
		},
	};

	client = new LanguageClient(
		'phplsrs',
		'PHP Language Server',
		serverOptions,
		clientOptions
	);

	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
