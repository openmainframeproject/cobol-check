grammar Gherkin;
startRule : gherkinDocument;
gherkinDocument : feature? EMPTY*? EOF ;

noline : (comment EMPTY | EMPTY)+? ;
feature : noline* featureHeader (EMPTY background)? (EMPTY scenarioDefinition)* (EMPTY rule)* ;
featureHeader : (LANGUAGE EMPTY)? tags? featureLine descriptionHelper? ;

rule : noline* ruleHeader background? scenarioDefinition* ;
ruleHeader : tags? ruleLine descriptionHelper? ;

background : noline* backGroundLine descriptionHelper? step* ;

scenarioDefinition : noline* tags? scenario ;
scenario : scenarioLine descriptionHelper? (step (EMPTY step)*)* examplesDefinition? ;
examplesDefinition : noline* examplesLine (EMPTY tags)? descriptionHelper examplesTable ;
examplesTable : TABLEROW (EMPTY TABLEROW)* ;

step : stepLine (EMPTY stepArg)? ;
stepArg : (dataTable | docString) ;

dataTable : noline* TABLEROW (noline+ TABLEROW)* ;
docString : docStringSeparator EMPTY (other EMPTY)* docStringSeparator ;

tags : TAGLINE (EMPTY TAGLINE)* ;


scenarioLine :  (SCENARIO | EXAMPLE) (OUTLINE)? COLON other? ;
examplesLine :  (SCENARIOS | EXAMPLES) COLON other? ;
featureLine : FEATURE COLON other? ;
backGroundLine : BACKGROUND COLON other? ;
stepLine : (GIVEN | WHEN | THEN | AND | BUT) other? ;
ruleLine : RULE COLON other? ;
comment : POUND other? ;
docStringSeparator : DOCSTRING other? ;
descriptionHelper : EMPTY (description noline+)*? ;
description : other ;

other : (ANY | BACKGROUND | EXAMPLE | EXAMPLES | FEATURE | OUTLINE | RULE | SCENARIO | SCENARIOS)
(ANY | AND | BACKGROUND | BUT | EXAMPLE | EXAMPLES | FEATURE | GIVEN | OUTLINE | RULE |
  SCENARIO | SCENARIOS | THEN | WHEN | COLON)*;

TABLEROW : WS? ('|'((~[|\r\n])|('\\\\|'))*)+(~[\\]'|');
TAGLINE : WS? AT ANY* EMPTY ;


AND : A N D ;
BACKGROUND : B A C K G R O U N D ;
BUT : B U T ;
EXAMPLE : E X A M P L E ;
EXAMPLES : E X A M P L E S ;
FEATURE : F E A T U R E ;
GIVEN : G I V E N ;
OUTLINE : O U T L I N E ;
RULE : R U L E ;
SCENARIO : S C E N A R I O ;
SCENARIOS : S C E N A R I O S ;
THEN : T H E N ;
WHEN : W H E N ;

LANGUAGE : WS? '#' WS? L A N G U A G E WS? ':'[a-zA-Z- ]+ ;
EMPTY : ENDLINE ;
WS : [ \t]+ -> channel(HIDDEN);

DOCSTRING :  ('"""' | '\'\'\'') ;
POUND : '#' ;
COLON : ':' ;
ANY: ~[ #:\t\n\r]+ ;
fragment AT: '@' ;
// case insensitive chars
fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');
fragment ENDLINE:('\n' | '\r');