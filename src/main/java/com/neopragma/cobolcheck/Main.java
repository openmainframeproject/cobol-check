package com.neopragma.cobolcheck;

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
