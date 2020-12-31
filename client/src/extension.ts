import * as path from 'path';
import * as fs from 'fs';
import * as http from 'http';
import * as https from 'https';

import { workspace, ExtensionContext, IndentAction, languages, window, Uri, TextDocument } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
	Executable,
	NotificationType
} from 'vscode-languageclient';

let client: LanguageClient;
  
/**
 * Downloads file from remote HTTP[S] host and puts its contents to the
 * specified location.
 */
async function download(url, filePath) {
  const proto = !url.charAt(4).localeCompare('s') ? https : http;

  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(filePath);
    let fileInfo = null;

    const request = proto.get(url, response => {
      if (response.statusCode !== 200) {
        reject(new Error(`Failed to get '${url}' (${response.statusCode})`));
        return;
      }

      fileInfo = {
        mime: response.headers['content-type'],
        size: parseInt(response.headers['content-length'], 10),
      };

      response.pipe(file);
    });

    // The destination stream is ended by the time it's called
    file.on('finish', () => resolve(fileInfo));

    request.on('error', err => {
      fs.unlink(filePath, () => reject(err));
    });

    file.on('error', err => {
      fs.unlink(filePath, () => reject(err));
    });

    request.end();
  });
}

export async function activate(context: ExtensionContext) {
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

	let downloadPath = 'https://';
	let storagePath = context.globalStoragePath;
	let binary = storagePath + '/phplsrs-0.1.1';
	if (!fs.existsSync(storagePath)) {
		fs.mkdirSync(storagePath);
	}

	if (!fs.existsSync(binary)) {
		
		window.setStatusBarMessage("Downloading client ...");
		await download(downloadPath, binary);
		
		window.setStatusBarMessage("Downloaded client");
	} else {	
		window.setStatusBarMessage("Client already installed.");
	}

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
