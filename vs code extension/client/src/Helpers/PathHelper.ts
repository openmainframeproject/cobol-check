import path = require('path');

export function couldBePath(value:string) :boolean{
	return value !== path.basename(value);
}

export function endsPath(path:string) : boolean{
	return (path.endsWith(".cut") || (path.endsWith(":") && path.includes(".cut")) || 
			path.endsWith(".CUT") || (path.endsWith(":") && path.includes(".CUT")) || 
			path.endsWith(".cob") || (path.endsWith(":") && path.includes(".cob")) ||
			path.endsWith(".COB") || (path.endsWith(":") && path.includes(".COB")) ||  
			path.endsWith(".cbl") || (path.endsWith(":") && path.includes(".cbl")) || 
			path.endsWith(".CBL:") || (path.endsWith(":") && path.includes(".CBL:")));
}