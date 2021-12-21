package com.neopragma.cobolcheck.features.launcher;

import com.neopragma.cobolcheck.exceptions.IOExceptionProcessingTestResultFile;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
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

    public void writeProcessOutputToTestResultsFile(Process proc, boolean outputToConsole) {
        getProcessOut(proc);
        writeOutPutToConsole();
        writeWasSuccesful = writeProcessOutputToFile(testResultsFilePath);
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
}
