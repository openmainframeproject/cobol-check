package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import java.util.ArrayList;
import java.util.List;

public class VerifyMockCount{
    private Mock attachedMock;
    private String expectedCount = "0";
    private boolean atLeast;
    private boolean noMoreThan;
    private String identifier;
    private String type;
    private List<String> arguments;

    private String testSuiteFileName;
    private int declarationLineNumberInOriginalFile;
    private int declarationIndexNumberInOriginalFile;

    public VerifyMockCount(){
        arguments = new ArrayList<>();
    }

    public String getExpectedCount() {
        return expectedCount;
    }

    public boolean isSetToAtLeast() {
        return atLeast;
    }

    public boolean isSetToNoMoreThan() {
        return noMoreThan;
    }

    public Mock getAttachedMock() {
        return attachedMock;
    }

    public String getIdentifier() {
        return identifier;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setIdentifier(String identifier) {
        this.identifier = identifier;
    }

    public void setAttachedMock(Mock attachedMock) {
        this.attachedMock = attachedMock;
    }

    public void setExpectedCount(String expectedCount) { this.expectedCount = expectedCount; }

    public String getTestSuiteFileName() {
        return testSuiteFileName;
    }

    public void setTestSuiteFileName(String testSuiteFileName) {
        this.testSuiteFileName = testSuiteFileName;
    }

    public int getDeclarationLineNumberInOriginalFile() {
        return declarationLineNumberInOriginalFile;
    }

    public void setDeclarationLineNumberInOriginalFile(int declarationLineNumberInOriginalFile) {
        this.declarationLineNumberInOriginalFile = declarationLineNumberInOriginalFile;
    }

    public int getDeclarationIndexNumberInOriginalFile() {
        return declarationIndexNumberInOriginalFile;
    }

    public void setDeclarationIndexNumberInOriginalFile(int declarationIndexNumberInOriginalFile) {
        this.declarationIndexNumberInOriginalFile = declarationIndexNumberInOriginalFile;
    }

    void expectExact(String expectedCount){
        this.expectedCount = expectedCount;
        atLeast = false;
        noMoreThan = false;
    }

    void expectAtLeast(String expectedCount){
        this.expectedCount = expectedCount;
        atLeast = true;
        noMoreThan = false;
    }

    void expectNoMoreThan(String expectedCount){
        this.expectedCount = expectedCount;
        atLeast = false;
        noMoreThan = true;
    }

    public List<String> getArguments() {
        return arguments;
    }

    public void addArgument(String argument) {
        argument = argument.replaceAll("\\s+", " ");
        arguments.add(argument);
    }
}
