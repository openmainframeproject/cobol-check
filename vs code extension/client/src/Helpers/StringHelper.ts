


export function insertTextAfterFirstToken(originalText:string, token:string, insertText:string) : string{
	let newText:string = "";
	let tokenIndex = originalText.indexOf(token) + token.length;
	newText = originalText.substring(0, tokenIndex) + "\n";
	newText += insertText + "\n";
	newText += originalText.substring(tokenIndex + 1);
	return newText;
}

//Expects string to be replaced to only be one character 
export function replaceAll(line:string, replace:string, replacement:string) : string{
	let characters = line.split('');
	let newLine = "";
	for (let char of characters){
		if (char === replace){
			newLine += replacement;
		}
		else{
			newLine += char;
		}
	}
	return newLine;
}