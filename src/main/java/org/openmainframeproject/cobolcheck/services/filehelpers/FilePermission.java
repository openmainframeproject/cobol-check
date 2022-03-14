package org.openmainframeproject.cobolcheck.services.filehelpers;

import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.log.Log;

import java.io.File;

public class FilePermission {

    public static void setFilePermissionForAllUsers(String path, String permissions) {
        setFilePermissionForAllUsers(new File(path), permissions);
    }

    public static void setFilePermissionForAllUsers(File file, String permissions){
        boolean read, write, execute = true;
        if (permissions == null || permissions.isEmpty())
            return;

        try{
            if (permissions.contains("r"))
                read = file.setReadable(true, false);
            else
                read = file.setReadable(true, true);

            if (permissions.contains("w"))
                write = file.setWritable(true, false);
            else
                write = file.setWritable(true, true);

            if (permissions.contains("x"))
                execute = file.setExecutable(true, false);
            else
                execute = file.setExecutable(true, true);

            if (read && write && execute)
                Log.debug("File permissions for all users set to <" + permissions + "> for file <" + file.getAbsolutePath() + ">");
            else
                throw new SecurityException();
        } catch (SecurityException e){
            Log.warn(Messages.get("WRN008", file.getAbsolutePath()));
        }
    }
}
