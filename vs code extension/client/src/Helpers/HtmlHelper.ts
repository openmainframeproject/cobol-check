import { couldBePath, endsPath } from './PathHelper';
import { replaceAll } from './StringHelper';


export function parseAsHtmlDocument(message : string, withFormattedLinks:boolean) : string{
	return "<!DOCTYPE html>\n" +
	"                <html lang=\"en\">\n" +
	"                    <head>\n" +
	"                        <meta charset=\"UTF-8\">\n" +
	"                        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n" +
	"                    </head>\n" +
	"                    <body>\n" +
							getFormattedHtmlSpan(message, withFormattedLinks) +
	" 					 </body>\n" +
	"                </html>";
}

export function getFormattedHtmlSpan(text:string, withFormattedLinks:boolean) : string{
	text = replaceAll(text, "<", "&lt;");
	text = replaceAll(text, ">", "&gt;");
	if (withFormattedLinks)
		text = makePathsIntoHtmlLinks(text);
	return 	"						<span style=\"white-space: pre-wrap;\">\n" +
										text +
			"						</span>\n";
}

export function getHtmlWarningDiv(insideText:string) : string{
	return "<div class=\"testcaseResult unittestResultHeader\" style=\"background-color: #a4a428;\">WARNINGS (hover for details)" +
		   "	<div class=\"testcaseResultArtifactDetails\" style=\"background-color: #a4a428;\">" +
					insideText +
		   "	</div>" +
		   "</div>"
}

export function makePathsIntoHtmlLinks(input:string) : string{
	let withLinks:string = "";
	let currentPossibleLink = "";

	let tokens = replaceAll(input, "\n", " \n ").split(" ");
	for (let token of tokens){
		if (currentPossibleLink !== ""){
			let combinedTokens = currentPossibleLink + " " + token;
			if (couldBePath(combinedTokens) && endsPath(combinedTokens)){
				token = combinedTokens;
				currentPossibleLink = "";
			}
			else{
				currentPossibleLink = combinedTokens;
				continue;
			}
		}
		if (couldBePath(token)){
			if (endsPath(token)){
				withLinks += "<a href=\" vscode://file/" + replaceAll(token, "#", "%23") + "10\">" + token + "</a> "
			}
			else{
				currentPossibleLink = token;
			}
		}
		else{
			withLinks += token + " ";
		}			
	}
	return withLinks;
}