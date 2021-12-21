///*
//Copyright 2020 David Nicolette
//
//Licensed under the Apache License, Version 2.0 (the "License");
//you may not use this file except in compliance with the License.
//You may obtain a copy of the License at
//
//http://www.apache.org/licenses/LICENSE-2.0
//
//Unless required by applicable law or agreed to in writing, software
//distributed under the License is distributed on an "AS IS" BASIS,
//WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//See the License for the specific language governing permissions and
//limitations under the License.
//*/
//package com.neopragma.cobolcheck;
//
//import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
//
//import java.io.*;
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.nio.file.Paths;
//import java.util.List;
//import java.util.Locale;
//
///**
// * Main class for command-line execution.
// *
// * @author Dave Nicolette (neopragma)
// * @since 14
// */
//public class Driver {
//    //Writing pertaining to process
//    if (launcher != null){
//        Process process = launcher.run(testSourceOutPath.toString());
//
//        String testResultsFilePath = config.getTestResultFilePathString();
//        try{
//            writeProcessOutputToFile(process, testResultsFilePath, true);
//            Log.info(messages.get("INF011", matchingDirectory, testResultsFilePath));
//        } catch (IOException testResultsFileNotFound){
//            throw new TestResultsInputFileNotFoundException(
//                    messages.get("ERR030", config.getTestResultFilePathString()));
//        }
//
//        int exitCode = 1;
////                    try {
//        exitCode = process.waitFor();
////                    } catch (InterruptedException interruptedException) {
////                        exitCode = 1;
////                    }
//        Log.info(messages.get("INF009", processName, String.valueOf(exitCode)));
//    }
//    //Initialize
//
//
//    //Write method
//    public static void writeProcessOutputToFile(Process proc, String path, boolean outputToConsole) throws IOException{
//        BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()));
//        BufferedReader stdError = new BufferedReader(new InputStreamReader(proc.getErrorStream()));
//
//        FileWriter fw = new FileWriter(path, true);
//        BufferedWriter bw = new BufferedWriter(fw);
//        String s = null;
//
//        while ((s = stdInput.readLine()) != null){
//            bw.write(s);
//            bw.newLine();
//            if (outputToConsole){
//                System.out.println(s);
//            }
//        }
//
//        while ((s = stdError.readLine()) != null){
//            if (outputToConsole){
//                System.out.println(s);
//            }
//
//        }
//        bw.close();
//    }
//
//    //Main is this used somehow???
//    public static void main(String[] args) throws InterruptedException {
//        messages = new Messages();
//        Config config = new Config(messages);
//        config.load();
//        Driver app = new Driver(
//                config,
//                new GetOpt(args, optionSpec, config));
//        app.run();
//        System.exit(exitStatus);
//    }
//
//
//}
