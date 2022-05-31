package org.openmainframeproject.cobolcheck.features.launcher.Formatter.DataTransferObjects;

import org.openmainframeproject.cobolcheck.services.Constants;

public class TableDocumentDto extends TableEmbedDto{

    @Override
    public Object getDataTransferObject() {
        String embedHtml = (String)super.getDataTransferObject();
        return getHtmlHeader() + Constants.NEWLINE + embedHtml + Constants.NEWLINE + getHtmlEnd();
    }

    private String getHtmlHeader(){
        return  "<!DOCTYPE html>\n" +
                "                <html lang=\"en\">\n" +
                "                    <head>\n" +
                "                        <meta charset=\"UTF-8\">\n" +
                "                        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n" +
                "                        <style>\n" +
                "                            td.GREEN {\n" +
                "                                border-left: 5px solid green;\n" +
                "                                color: forestgreen;\n" +
                "                            }\n" +
                "                            td.RED {\n" +
                "                                border-left: 5px solid red;\n" +
                "                                color: indianred;\n" +
                "                            }\n" +
                "                            td.GREY {\n" +
                "                                border-left: 5px solid grey;\n" +
                "                                color: darkgrey;\n" +
                "                            }\n" +
                "                            table.testcaseResultTable {\n" +
                "                                width: 100%;\n" +
                "                            }\n" +
                "                            table.testcaseResultTable td {\n" +
                "                                width: 50%;\n" +
                "                            }\n" +
                "\n" +
                "                            div.testcaseResult {\n" +
                "                                padding: 5px;\n" +
                "                                border-radius: 5px;\n" +
                "                                font-weight: 600;\n" +
                "                                margin-bottom: 5px;\n" +
                "                            }\n" +
                "\n" +
                "                            div.unittestResultHeader {\n" +
                "                                background-color: #F7F3F7;\n" +
                "                                color: #B5AEB5;\n" +
                "                            }\n" +
                "\n" +
                "                            body.vscode-dark div.passedResult {\n" +
                "                                color: #EFF7CE;\n" +
                "                                background-color: #6B9239;\n" +
                "                            }\n" +
                "\n" +
                "                            body.vscode-dark div.failedResult {\n" +
                "                                background-color: #CE4542;\n" +
                "                                color: #FFC3C6;\n" +
                "                            }\n" +
                "\n" +
                "                            body.vscode-dark div.unittestResultHeader {\n" +
                "                                color: #F7F3F7;\n" +
                "                                background-color: #B5AEB5;\n" +
                "                            }\n" +
                "\n" +
                "                            div.failedResult {\n" +
                "                                background-color: #FFC3C6;\n" +
                "                                color: #CE4542;\n" +
                "                            }\n" +
                "\n" +
                "                            div.passedResult {\n" +
                "                                background-color: #EFF7CE;\n" +
                "                                color: #6B9239;\n" +
                "                            }\n" +
                "\n" +
                "                            div.unittestResultHeader div.testcaseResultArtifactDetails {\n" +
                "                                display:none;\n" +
                "                            }\n" +
                "\n" +
                "                            div.unittestResultHeader:hover div.testcaseResultArtifactDetails {\n" +
                "                                display:block;\n" +
                "                            }\n" +
                "                        </style>\n" +
                "                    </head>\n" +
                "                    <body>";
    }

    private String getHtmlEnd(){
        return " </body>\n" +
                "                </html>";
    }
}
