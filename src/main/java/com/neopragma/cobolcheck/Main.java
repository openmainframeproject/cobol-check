package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.GetOpt;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.workers.Driver;

class Main {
    public static void main(String[] args) throws InterruptedException {
        Messages messages = new Messages();
        Config config = new Config(messages);
        config.load();
        Driver app = new Driver(
                config,
                new GetOpt(args, Constants.COMMAND_lINE_OPTIONS, config));
        app.run();
        System.exit(app.getExitStatus());
    }
}
