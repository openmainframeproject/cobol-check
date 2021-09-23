package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.argumentHandler.ArgumentHandler;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.workers.Initializer;

class Main {
    public static void main(String[] args) throws InterruptedException {
        Config.load();
        Initializer app = new Initializer(new ArgumentHandler(args, Constants.COMMAND_lINE_OPTIONS));
        app.run();
        System.exit(app.getExitStatus());
    }
}
