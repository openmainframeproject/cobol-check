package org.openmainframeproject.cobolcheck.features.launcher;

import org.openmainframeproject.cobolcheck.features.launcher.Formatter.DataTransferObjects.DataTransferObjectStyle;
import org.openmainframeproject.cobolcheck.features.launcher.Formatter.Formats.Formatter;
import org.openmainframeproject.cobolcheck.features.launcher.Formatter.Formats.HTMLFormat;
import org.openmainframeproject.cobolcheck.features.launcher.Formatter.Formats.TestOutputFormat;
import org.openmainframeproject.cobolcheck.features.launcher.Formatter.Formats.XMLFormat;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.filehelpers.FilePermission;
import org.openmainframeproject.cobolcheck.services.log.Log;

import java.io.*;

public class ProcessOutputWriter {

    String testResultsFilePath;
    boolean writeWasSuccesful = false;
    String processInput;
    String processError;
    XMLFormat xmlFormat;
    HTMLFormat htmlFormat;

    public ProcessOutputWriter() {
        testResultsFilePath = Config.getTestResultFilePathString();
        cleanupOldTestResults();
    }

    public String getTestResultsFilePath() {
        return testResultsFilePath;
    }

    public boolean WriteWasSuccesful() {
        return writeWasSuccesful;
    }

    public void writeProcessOutputToTestResultsFile(Process proc, TestOutputFormat format, DataTransferObjectStyle style,
                                                    String programName, boolean outputToConsole, boolean isLastRun) {
        getProcessOut(proc);
        if (outputToConsole)
            writeOutPutToConsole();
        switch (format){
            case txt:
                writeWasSuccesful = writeProcessOutputToFile(testResultsFilePath);
                break;
            case xml:
                if (xmlFormat == null)
                    xmlFormat = new XMLFormat(style);
                writeProcessOutputWithFormat(xmlFormat, programName, isLastRun);
                break;
            case html:
                if (htmlFormat == null)
                    htmlFormat = new HTMLFormat(style);
                writeProcessOutputWithFormat(htmlFormat, programName, isLastRun);
                break;
        }
        FilePermission.setFilePermissionForAllUsers(testResultsFilePath, Config.getGeneratedFilesPermissionAll());
    }

    private void getProcessOut(Process proc) {
        Reader reader = new InputStreamReader(proc.getInputStream());
        int maxBytesToReadFromCobolCheck = 25000;
        char tempReadBuffer[] = new char[maxBytesToReadFromCobolCheck];
        int writeOffset = 0;
        int numberOfCharsRead = 0;
        char cobolCheckOutput[] = null;
        try {
                numberOfCharsRead = reader.read(tempReadBuffer, writeOffset, maxBytesToReadFromCobolCheck);
                System.out.println("ProcessOutputWriter: numberOfCharsRead = " + numberOfCharsRead);
                if(numberOfCharsRead == maxBytesToReadFromCobolCheck) {
                    int largeMaxBytesToReadFromCobolCheck = 100000;
                    char largeTempReadBuffer[] = new char[maxBytesToReadFromCobolCheck + largeMaxBytesToReadFromCobolCheck];
                    System.arraycopy(tempReadBuffer, 0, largeTempReadBuffer, 0, tempReadBuffer.length);
                    int largeNumberOfCharsRead = reader.read(largeTempReadBuffer, tempReadBuffer.length, largeMaxBytesToReadFromCobolCheck);
                    System.out.println("ProcessOutputWriter: largeNumberOfCharsRead = " + largeNumberOfCharsRead);
                    numberOfCharsRead += largeNumberOfCharsRead;
                    cobolCheckOutput = new char[numberOfCharsRead];
                    System.arraycopy(largeTempReadBuffer, 0, cobolCheckOutput, 0, numberOfCharsRead);
                }
                else {
                    cobolCheckOutput = new char[numberOfCharsRead];
                    System.arraycopy(tempReadBuffer, 0, cobolCheckOutput, 0, numberOfCharsRead);
                }
                reader.close();
                System.out.println("ProcessOutputWriter: numberOfCharsRead = " + numberOfCharsRead);
        }
        catch(IOException e) {
            System.out.println("ProcessOutputWriter - getProcessOut - e = " + e);
        }
        
        BufferedReader stdError = new BufferedReader(new InputStreamReader(proc.getErrorStream()));

        processError = "";
        try{
            String s = null;
            processInput = "";
            for (int i = 0; i < numberOfCharsRead; i++) {
                processInput += cobolCheckOutput[i];
            }
            while ((s = stdError.readLine()) != null){
                if (s != null)
                    processError += s + Constants.NEWLINE;
            }
            //Remove extra NEWLINE:
            processInput = StringHelper.removeLastIndex(processInput);
            processError = StringHelper.removeLastIndex(processError);

            stdError.close();
        }
        catch (IOException ex) {
            Log.warn(Messages.get("WRN007"));
        }

        // Convert StringBuilder to String, removing the last NEWLINE if necessary
        processInput = StringHelper.removeLastIndex(processInputBuilder.toString());
        processError = StringHelper.removeLastIndex(processErrorBuilder.toString());
    }


    private void writeOutPutToConsole() {
        System.out.println(processInput);
        System.out.println(processError);
    }


    private boolean writeProcessOutputToFile(String path) {
        try{
            FileWriter fw = new FileWriter(path, true);
            BufferedWriter bw = new BufferedWriter(fw);
            String[] lines = processInput.split(Constants.NEWLINE);
            for (String line : lines){
                bw.write(line);
                bw.newLine();
            }

            bw.close();
            return true;
        }
        catch (IOException ex)
        {
            Log.warn(Messages.get("WRN006", path));
            return false;
        }
    }

    private void writeProcessOutputWithFormat(Formatter formatter, String programName, boolean writeToFile){
        formatter.parseText(processInput, programName);

        if (writeToFile) {
            try {
                formatter.writeInFormat(testResultsFilePath);
                writeWasSuccesful = true;
            } catch (Exception e) {
                Log.error(e.getMessage());
                writeWasSuccesful = false;
            }
        }
    }

    private void cleanupOldTestResults() {
        //Clear current testResults file
        try {
            if (testResultsFilePath.contains(Constants.FILE_SEPARATOR)){
                String dir = testResultsFilePath.substring(0, testResultsFilePath.lastIndexOf(Constants.FILE_SEPARATOR));
                new File(dir).mkdir();
            }
            new PrintWriter(testResultsFilePath).close();
            Log.info(Messages.get("INF010", testResultsFilePath));
        } catch (FileNotFoundException ex){
            Log.warn(Messages.get("WRN005", testResultsFilePath));
        }
        //If any other test results files with a different file extension still exists, delete them
        for (TestOutputFormat extension : TestOutputFormat.values()){
            File oldTestResultsFile = new File(StringHelper.changeFileExtension(testResultsFilePath, extension.name()));
            if (oldTestResultsFile.exists() && !oldTestResultsFile.getPath().equals(testResultsFilePath))
                oldTestResultsFile.delete();
        }
    }
}
