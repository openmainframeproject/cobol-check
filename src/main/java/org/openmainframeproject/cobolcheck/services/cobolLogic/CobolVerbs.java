package org.openmainframeproject.cobolcheck.services.cobolLogic;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;

/**
 * Helper class for parsing tokens from the test suite. Users may embed standard Cobol statements in test
 * cases to set up preconditions and to define the behavior of mocks. This class provides a lookup of
 * Cobol verbs so we can identify the start of a Cobol statement. Verbs that are not useful for those
 * purposes are omitted from the list (e.g., EJECT, ENTER). Verbs that would cause a hard branch out of the
 * test logic or terminate the test run are omitted (e.g., ALTER, GO, GOBACK, RETURN).
 *
 * @author neopragma
 * @since 1.8
 */
public class CobolVerbs {
    private static final List<String> values = Arrays.asList(
            "ACCEPT",
            "ADD",
            "CALL",
            "CLOSE",
            "COMPUTE",
            "CONTINUE",
            "COPY",
            "DELETE",
            "DISPLAY",
            "DISPLAY-1",
            "DIVIDE",
            "EVALUATE",
            "GENERATE",
            "INITIALIZE",
            "INVOKE",
            "MOVE",
            "MULTIPLY",
            "OPEN",
            "PERFORM",
            "READ",
            "REWRITE",
            "SEARCH",
            "SET",
            "SORT",
            "START",
            "STOP",
            "STRING",
            "SUBTRACT",
            "UNSTRING",
            "WRITE"
    );

    private static final List<String> allVerbs = Arrays.asList(
            "ACCEPT",
            "ADD",
            "ALTER",
            "CALL",
            "CANCEL",
            "CHAIN",
            "CLOSE",
            "COMMIT",
            "COMPUTE",
            "CONTINUE",
            "COPY",
            "DELETE",
            "DISABLE",
            "DISPLAY",
            "DISPLAY-1",
            "DIVIDE",
            "ELSE",
            "ENABLE",
            "END",
            "END-EVALUATE",
            "END-IF",
            "END-INVOKE",
            "END-PERFORM",
            "END-SET",
            "ENTER",
            "ENTRY",
            "EVALUATE",
            "EXAMINE",
            "EXEC",
            "EXECUTE",
            "EXHIBIT",
            "EXIT",
            "GENERATE",
            "GO",
            "GOBACK",
            "IF",
            "INITIALIZE",
            "INITIATE",
            "INSPECT",
            "INVOKE",
            "MERGE",
            "MOVE",
            "MULTIPLY",
            "NEXT",
            "NOTE",
            "ON",
            "OPEN",
            "OTHERWISE",
            "PERFORM",
            "READ",
            "READY",
            "RECEIVE",
            "RECOVER",
            "RELEASE",
            "RESET",
            "RETURN",
            "REWRITE",
            "ROLLBACK",
            "SEARCH",
            "SEND",
            "SERVICE",
            "SET",
            "SORT",
            "START",
            "STOP",
            "STRING",
            "SUBTRACT",
            "SUPPRESS",
            "TERMINATE",
            "TRANSFORM",
            "UNLOCK",
            "UNSTRING",
            "WHEN",
            "WRITE"
    );

    public static boolean isCobolVerb(String token) {
        return values.contains(token.toUpperCase(Locale.ROOT));
    }

    public static boolean isStartOrEndCobolVerb(String token){
        return allVerbs.contains(token.toUpperCase(Locale.ROOT));
    }

    private CobolVerbs() {
        throw new IllegalStateException("Can't instantiate static class");
    }

}
