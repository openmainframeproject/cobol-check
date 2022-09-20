package org.openmainframeproject.cobolcheck.services;

/**
 * Current version of the product.
 *
 * @author Dave Nicolette (neopragma)
 * @since 1.8
 */
public class Version {
    public static String current() {
        String version = Version.class.getPackage().getImplementationVersion();
        if (version != null)
            return version;
        else
            return "1.0.DEV";
    }
}
