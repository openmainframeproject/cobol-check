package org.openmainframeproject.cobolcheck.services.filehelpers;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Locale;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.log.Log;
import org.openmainframeproject.cobolcheck.services.platform.Platform;
import org.openmainframeproject.cobolcheck.services.platform.PlatformLookup;

public class EncodingIO {
    public static Reader getReaderWithCorrectEncoding(String path) throws IOException {
        InputStream inputStream = new FileInputStream(path);
        BufferedReader bufferedReader = null;
        String fileName = new File(path).getName();
        String charset = Config.getCharsetForPlatform(PlatformLookup.get());

        if (charset == null || charset.trim().toUpperCase(Locale.ROOT).equals("DEFAULT")){
            bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
            Log.debug("EncodingIO.getReaderWithCorrectEncoding() Encoding set to <" + Charset.defaultCharset() +
                    "> for reader: " + fileName);
        }
        else {
            bufferedReader = new BufferedReader(new InputStreamReader(inputStream, charset));
            Log.debug("EncodingIO.getReaderWithCorrectEncoding() Encoding set to <" + charset + "> for reader: " + fileName);
        }
        return bufferedReader;
    }

    public static Writer getWriterWithCorrectEncoding(String path) throws IOException {
        return getWriterWithCorrectEncoding(path, false);
    }
    public static Writer getWriterWithCorrectEncoding(String path, boolean append) throws IOException {
        OutputStream outputStream = new FileOutputStream(path, append);
        BufferedWriter bufferedWriter = null;
        String fileName = new File(path).getName();
        String charset = Config.getCharsetForPlatform(PlatformLookup.get());

        if (charset == null || charset.trim().toUpperCase(Locale.ROOT).equals("DEFAULT")) {
            bufferedWriter = new BufferedWriter(new OutputStreamWriter(outputStream));
            Log.debug("EncodingIO.getWriterWithCorrectEncoding() Encoding set to <" + Charset.defaultCharset() +
                    "> for writer: " + fileName);
        } else {
            bufferedWriter = new BufferedWriter(new OutputStreamWriter(outputStream, charset));
            Log.debug("EncodingIO.getWriterWithCorrectEncoding() Encoding set to <" + charset + "> for writer: " + fileName);
        }
        return bufferedWriter;
    }
}
