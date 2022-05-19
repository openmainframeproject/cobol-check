import * as vscode from 'vscode';


export function getConfigurationMap(configurationPath : string, callBack: (configurationMap : Map<string, string>) => any){
	const fs = require('fs');

	fs.readFile(configurationPath, 'utf8', function(err, data) {
		if(err) throw err;

		const arr = data.toString().replace(/\r\n/g,'\n').split('\n');
		var configurations = new Map<string,string>();

		for(let line of arr) {
			line = line.trim();
			if (line !== "" && !line.startsWith('#')){
				let keyValue = line.split('=');
				if (keyValue.length > 1){
					let key = keyValue[0].trim();
					let value = keyValue[1].trim();
					configurations.set(key, value);
				}
			}
		}
		callBack(configurations);
	});
}

export function setConfiguration(configurationPath : string, key : string, newValue : string){
	const fs = require('fs');

	fs.readFile(configurationPath, 'utf8', function(err, data) {
		if(err) throw err;

		const arr = data.toString().replace(/\r\n/g,'\n').split('\n');
		var newConfigurationText = "";
		var keyHasBeenFound = false;

		for(let line of arr) {
			var trimmedLine = line.trim();
			if (trimmedLine === "" && trimmedLine.startsWith('#')){
				newConfigurationText += line + '\n';
				continue;
			}

			let keyValue = trimmedLine.split('=');
			if (keyValue.length > 1 && keyValue[0].trim() === key){
				newConfigurationText += key + ' = ' + newValue;
				keyHasBeenFound = true;
			} else{
				newConfigurationText += line + '\n';
			}
		}

		if (!keyHasBeenFound){
			vscode.window.showErrorMessage('Could not find key: ' + key + ' in config file: ' + configurationPath)
		}
		fs.writeFile(configurationPath, newConfigurationText, 'utf8', function (err) {
			if (err) return console.log(err);
		 });
	});
}

export function resetConfigurations(configurationPath : string, defaultConfigurationPath : string){
	const fs = require('fs');

	fs.readFile(defaultConfigurationPath, 'utf8', function(err, data) {
		if(err) throw err;

		const newConfigurationText = data.toString();

		fs.writeFile(configurationPath, newConfigurationText, 'utf8', function (err) {
			if (err) return console.log(err);
		 });
	});
}

export function getConfigurationValueFor(configurationPath : string, key : string) : Promise <string>{
	const fs = require('fs');

	return new Promise(async resolve => {

		fs.readFile(configurationPath, 'utf8', function(err, data) {
			if(err) throw err;
	
			const arr = data.toString().replace(/\r\n/g,'\n').split('\n');
	
			for(let line of arr) {
				line = line.trim();
				if (line !== "" && !line.startsWith('#')){
					let keyValue = line.split('=');
					if (keyValue.length > 1){
						let _key = keyValue[0].trim();
						let _value = keyValue[1].trim();
						if (_key === key){
							resolve(_value);
							// callBack(_value);
							// return;
						}
					}
				}
			}
			resolve(null);
		});

	 });
}
