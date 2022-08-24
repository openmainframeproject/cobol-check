export enum OutputState {
	NotSet,
	NoIssues,
	SyntaxWarnings,
	SyntaxError,
	JavaException,
	CmdError,
	CobolCompilerError
  }


export class CobolCheckOutputParser{
	private stdout : string;
	private stderr : string;
	private error : string;

	private syntaxErrorToken:string = "SYNTAX ERROR in file:";
	private runTimeErrorToken:string = "RUNTIME ERROR in file:";
	private warningToken:string = "WARNING in file:";
	private testSuiteToken:string = "TESTSUITE:";
	private exceptionToken:string = "Exception in thread \"";
	private errorToken:string = "Error: ";

	state:OutputState = OutputState.NotSet;
	outputText:string = "";

	constructor(stdout:string, stderr:string, error:string){
		this.stdout = stdout;
		this.stderr = stderr;
		this.error = error;

		//Parse syntax errors/warnings
		if (stdout !== null && stdout !== ''){
			const arr = stdout.toString().replace(/\r\n/g,'\n').split('\n');
			let reachedTestResults:boolean = false;
			for(let line of arr) {
				line = line.trim();
				if (this.state === OutputState.CobolCompilerError){
					this.outputText += line + "\n";
					continue;
				}
				if (line.startsWith(this.syntaxErrorToken) || line.startsWith(this.runTimeErrorToken)){
					this.state = OutputState.SyntaxError;
				}
				else if (line.startsWith(this.warningToken) && this.state !== OutputState.SyntaxError){
					this.state = OutputState.SyntaxWarnings;
				}
				if (line.startsWith(this.testSuiteToken)){
					reachedTestResults = true;
					if (this.state !== OutputState.SyntaxWarnings)
						this.state = OutputState.NoIssues;
				}
				if (this.state === OutputState.SyntaxError || this.state === OutputState.SyntaxWarnings){
					if (!reachedTestResults)
						this.outputText += line + "\n";
				}
				if (line.startsWith(this.errorToken)){
					this.state = OutputState.CmdError;
					break;
				}
				if (this.state === OutputState.NotSet){
					this.state = OutputState.CobolCompilerError;
					this.outputText += line + "\n";
				}
			}
		}
		//Parsing Java exception and cmd exception
		if (error !== null && this.state !== OutputState.CobolCompilerError && 
			this.state !== OutputState.SyntaxError && this.state !== OutputState.SyntaxWarnings){
			const arr = error.toString().replace(/\r\n/g,'\n').split('\n');
			this.state = OutputState.CmdError;
			for(let line of arr) {
				line = line.trim();
				if (line.startsWith(this.exceptionToken)){
					this.state = OutputState.JavaException;
					this.outputText = line + "\n";
				}
				this.outputText += line + "\n";
			}
		}
	}

	getErrorMessage() : string{
		return "Cobol Check stopped due to " + OutputState[this.state];
	}
}