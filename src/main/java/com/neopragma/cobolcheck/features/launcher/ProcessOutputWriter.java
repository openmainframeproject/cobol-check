package com.neopragma.cobolcheck.features.launcher;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.log.Log;

import java.io.*;

public class ProcessOutputWriter {

    String testResultsFilePath;

    public ProcessOutputWriter() throws FileNotFoundException {
        testResultsFilePath = Config.getTestResultFilePathString();
        new PrintWriter(testResultsFilePath).close();
        Log.info(Messages.get("INF010", testResultsFilePath));
    }


    public void writeProcessOutputToTestResultsFile(Process proc, boolean outputToConsole) {
        writeProcessOutputToFile(proc, testResultsFilePath, outputToConsole);
    }

    public static void writeProcessOutputToFile(Process proc, String path, boolean outputToConsole) {
        BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()));
        BufferedReader stdError = new BufferedReader(new InputStreamReader(proc.getErrorStream()));

        try{
            FileWriter fw = new FileWriter(path, true);
            BufferedWriter bw = new BufferedWriter(fw);
            String s = null;

            while ((s = stdInput.readLine()) != null){
                bw.write(s);
                bw.newLine();
                if (outputToConsole){
                    System.out.println(s);
                }
            }

            while ((s = stdError.readLine()) != null){
                if (outputToConsole){
                    System.out.println(s);
                }

            }
            bw.close();
        }catch (IOException e)
        {
            throw new PossibleInternalLogicErrorException("Could not write to file: " + path);
        }

    }
}