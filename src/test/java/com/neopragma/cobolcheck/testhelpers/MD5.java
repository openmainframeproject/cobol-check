/*
Copyright 2020 David Nicolette

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
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
