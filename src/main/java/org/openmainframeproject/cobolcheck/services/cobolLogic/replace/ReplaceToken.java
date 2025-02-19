package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

public class ReplaceToken {
    private String value;
    private ReplaceTokenType type;

    public ReplaceToken(String value) {
        this.value = value.trim();
        this.setType();
    }

    private void setType() {
        switch (this.value.toUpperCase()) {
            case "REPLACE":
                this.type = ReplaceTokenType.REPLACE;
                break;
            case "LEADING":
                this.type = ReplaceTokenType.LEADING;
                break;
            case "TRAILING":
                this.type = ReplaceTokenType.TRAILING;
                break;
            case "BY":
                this.type = ReplaceTokenType.BY;
                break;
            case ".":
                this.type = ReplaceTokenType.TERMINATOR;
                break;
            default:
                this.type = ReplaceTokenType.OTHER;
                break;

        }
    }

    public String getValue() {
        return value;
    }

    public ReplaceTokenType getType() {
        return type;
    }
}
