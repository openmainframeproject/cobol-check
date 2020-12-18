package com.neopragma.cobolcheck;

/**
 * Main class for command-line execution.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Driver {

    private Config config;

    public Driver() {
        System.out.println("Driver constructor");

    }

    public static void main(String[] args) {
        System.out.println("Driver started");
        Driver app = new Driver();
        System.out.println("Driver terminating normally");
        System.exit(Constants.STATUS_NORMAL);
    }
}
