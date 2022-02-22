package com.neopragma.cobolcheck.features.launcher;

import com.neopragma.cobolcheck.exceptions.IOExceptionProcessingTestResultFile;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.features.Formatter.DataTransferObjects.DataTransferObjectStyle;
import com.neopragma.cobolcheck.features.Formatter.Formats.Formatter;
import com.neopragma.cobolcheck.features.Formatter.Formats.TestOutputFormat;
import com.neopragma.cobolcheck.features.Formatter.Formats.XMLFormat;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.log.Log;

import javax.xml.bind.JAXBException;
import java.io.*;
import java.util.Arrays;
import java.util.stream.Stream;

public class ProcessOutputWriter {

    String testResultsFilePath;
    boolean writeWasSuccesful = false;
    String processInput;
    String processError;
    XMLFormat xmlFormat;

    public ProcessOutputWriter() {
        testResultsFilePath = Config.getTestResultFilePathString();
        try {
            new PrintWriter(testResultsFilePath).close();
        } catch (FileNotFoundException ex){
            Log.warn(Messages.get("WRN005", testResultsFilePath));
        }

        Log.info(Messages.get("INF010", testResultsFilePath));
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
        cleanupOldTestResults();
        switch (format){
            case txt:
                writeWasSuccesful = writeProcessOutputToFile(testResultsFilePath);
                break;
            case XML:
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
            FileWriter fw = new FileWriter(StringHelper.changeFileExtension(path, "txt"), true);
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

        if (writeToFile)
        {
            try {
                formatter.getFormattedString(testResultsFilePath);
                writeWasSuccesful = true;
            } catch (Exception e) {
                writeWasSuccesful = false;
            }
        }
    }

    private void cleanupOldTestResults() {
        if (testResultsFilePath.contains("."))
            testResultsFilePath = testResultsFilePath.split("\\.")[0];

        File oldTestResultsFile = new File(testResultsFilePath);

//        int fileNameSplitIndex = testResultsFilePath.lastIndexOf(Constants.FILE_SEPARATOR);
//        String fileName = "";
//        String directoryPath = "";
//        if (fileNameSplitIndex == -1){
//            fileName = testResultsFilePath;
//            directoryPath = System.getProperty("user.dir");
//        }
//        else {
//            fileName = testResultsFilePath.substring(fileNameSplitIndex + 1);
//            directoryPath = testResultsFilePath.substring(0, fileNameSplitIndex);
//        }
//
//
//        File directory = new File(directoryPath);
//        String finalDirectoryPath = directoryPath;
//        File[] potentialTestResultFiles = Arrays.stream(directory.listFiles()).filter(file ->
//                file.getPath().substring(0, file.getPath().lastIndexOf(Constants.FILE_SEPARATOR))
//                        .equals(finalDirectoryPath)).toArray(File[]::new);
//
//        for (File file : potentialTestResultFiles){
//            if (file.getName().equals(fileName))
//                testResultsFilePath = file.getAbsolutePath();
//        }
    }
}
