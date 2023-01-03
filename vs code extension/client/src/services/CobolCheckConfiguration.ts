import * as vscode from 'vscode';
import * as LOGGER from '../utils/Logger'


export function getConfigurationMap(configurationPath : string, callBack: (configurationMap : Map<string, string>) => any){
	const fs = require('fs');

	fs.readFile(configurationPath, 'utf8', function(err, data) {
		if(err) {
			vscode.window.showErrorMessage("Got an error while trying to read from Config file:\n " + err);
			callBack(null);
			return;
		}

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
		if(err) {
			vscode.window.showErrorMessage("Got an error while trying to read from Config file:\n " + err);
			return;
		}

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
				newConfigurationText += key + ' = ' + newValue + '\n';
				keyHasBeenFound = true;
				LOGGER.log("Updated configuration key: " + key + " with value: " + newValue, LOGGER.INFO);
			} else{
				newConfigurationText += line + '\n';
			}
		}

		if (!keyHasBeenFound){
			vscode.window.showErrorMessage('Could not find key: ' + key + ' in config file: ' + configurationPath)
			return;
		}
		fs.writeFile(configurationPath, newConfigurationText, 'utf8', function (err) {
			if(err) {
				vscode.window.showErrorMessage("Got an error while trying to write to Config file:\n " + err);
				return;
			}
		 });
	});
}

export function resetConfigurations(configurationPath : string, defaultConfigurationPath : string){
	const fs = require('fs');

	fs.readFile(defaultConfigurationPath, 'utf8', function(err, data) {
		if(err) {
			vscode.window.showErrorMessage("Got an error while trying to read from Config file:\n " + err);
			return;
		}

		const newConfigurationText = data.toString();

		fs.writeFile(configurationPath, newConfigurationText, 'utf8', function (err) {
			if(err) {
				vscode.window.showErrorMessage("Got an error while trying to write to Config file:\n " + err);
				return;
			}else{
				LOGGER.log("Successfully reset configuration file", LOGGER.INFO)
			}
		 });
	});
}

export function getConfigurationValueFor(configurationPath : string, key : string) : Promise <string>{
	const fs = require('fs');

	return new Promise(async resolve => {

		fs.readFile(configurationPath, 'utf8', function(err, data) {
			if(err){
				vscode.window.showErrorMessage("Got an error while trying to read from Config file:\n " + err);
				resolve(null);
			}
	
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
						}
					}
				}
			}
			resolve(null);
		});

	 });
}
