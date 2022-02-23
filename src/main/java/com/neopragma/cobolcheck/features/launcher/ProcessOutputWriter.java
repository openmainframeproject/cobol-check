package com.neopragma.cobolcheck.features.launcher;

import com.neopragma.cobolcheck.features.launcher.Formatter.DataTransferObjects.DataTransferObjectStyle;
import com.neopragma.cobolcheck.features.launcher.Formatter.Formats.Formatter;
import com.neopragma.cobolcheck.features.launcher.Formatter.Formats.TestOutputFormat;
import com.neopragma.cobolcheck.features.launcher.Formatter.Formats.XMLFormat;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.log.Log;

import java.io.*;

public class ProcessOutputWriter {

    String testResultsFilePath;
    boolean writeWasSuccesful = false;
    String processInput;
    String processError;
    XMLFormat xmlFormat;

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
        }
    }

    private void getProcessOut(Process proc) {
        BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()));
        BufferedReader stdError = new BufferedReader(new InputStreamReader(proc.getErrorStream()));
        processInput = "";
        processError = "";

        try{
            String s = null;
            while ((s = stdInput.readLine()) != null){
                if (s != null)
                    processInput += s + Constants.NEWLINE;
            }

            while ((s = stdError.readLine()) != null){
                if (s != null)
                    processError += s + Constants.NEWLINE;
            }
            //Remove extra NEWLINE:
            processInput = StringHelper.removeLastIndex(processInput);
            processError = StringHelper.removeLastIndex(processError);

            stdInput.close();
            stdError.close();
        }
        catch (IOException ex)
        {
            Log.warn(Messages.get("WRN007"));
        }
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
