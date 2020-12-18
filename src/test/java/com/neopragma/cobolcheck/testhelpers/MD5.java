package com.neopragma.cobolcheck.testhelpers;

import java.io.FileInputStream;
import java.io.InputStream;
import java.security.MessageDigest;

/**
 * Compute MD5 hash of a file. Used in integration tests.
 *
 * @author Dave Nicolette
 * @since 1.8
 */
public class MD5 {

    public static String MD5HashFile(String filename) throws Exception {
        byte[] buf = ChecksumFile(filename);
        String res = "";
        for (byte b : buf) {
            res += Integer.toString((b & 0xff) + 0x100, 16).substring(1);
        }
        return res;
    }

    public static byte[]  ChecksumFile(String filename) throws Exception {
        InputStream fis = new FileInputStream(filename);
        byte[] buf = new byte[1024];
        MessageDigest complete = MessageDigest.getInstance("MD5");
        int n;
        do {
            n= fis.read(buf);
            if (n > 0) {
                complete.update(buf, 0, n);
            }
        } while (n != -1);
        fis.close();
        return complete.digest();
    }

}
