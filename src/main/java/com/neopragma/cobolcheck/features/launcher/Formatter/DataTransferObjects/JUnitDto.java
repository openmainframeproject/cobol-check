package com.neopragma.cobolcheck.features.launcher.Formatter.DataTransferObjects;

//Generated from https://jsonformatter.org/ (with some alterations)
// based on XML from: https://llg.cubic.org/docs/junit/

import javax.xml.bind.annotation.*;
import java.util.ArrayList;
import java.util.List;

public class JUnitDto extends DataTransferObject{
    private Testsuites testsuites = new Testsuites();

    public Testsuites getTestsuites() { return testsuites; }
    public void setTestsuites(Testsuites value) { this.testsuites = value; }

    public void setTestCounts()
    {
        int allTests = 0;
        int allFailures = 0;
        for (Testsuite testsuite : getTestsuites().getTestsuites()){
            int tests = 0;
            int failures = 0;
            for (Testcase testcase : testsuite.getTestcase()){
                tests++;
                if (testcase.getFailure() != null)
                    failures++;
            }
            testsuite.setTests(Integer.toString(tests));
            testsuite.setFailures(Integer.toString(failures));
            allTests += tests;
            allFailures += failures;
        }
        getTestsuites().setTests(Integer.toString(allTests));
        getTestsuites().setFailures(Integer.toString(allFailures));
    }

    @Override
    public void moveToNextTestSuite() {
        super.moveToNextTestSuite();
        getTestsuites().addTestSuite(new Testsuite());
        getTestsuites().getTestsuites().get(testSuiteIndex).setID(Integer.toString(testSuiteIndex));
    }

    @Override
    public void moveToNextTestCase() {
        super.moveToNextTestCase();
        getTestsuites().getTestsuites().get(testSuiteIndex).addTestCase(new Testcase());
    }

    @Override
    public Object getDataTransferObject() {
        setTestCounts();
        return getTestsuites();
    }

    @Override
    public void setNumberOfAllTests(String numberofTests) {
        //Not needed
    }

    @Override
    public void setNumberOffAllFailures(String numberOfFailures) {
        //Not needed
    }

    @Override
    public void setCurrentTestSuiteName(String name) {
        getTestsuites().getTestsuites().get(testSuiteIndex).setName(name);
    }

    @Override
    public void setCurrentTestSuiteTests(String numberofTests) {
        getTestsuites().getTestsuites().get(testSuiteIndex).setTests(numberofTests);
    }

    @Override
    public void setCurrentTestSuiteFailures(String numberOfFailures) {
        getTestsuites().getTestsuites().get(testSuiteIndex).setFailures(numberOfFailures);
    }

    @Override
    public void setCurrentTestSuitePackage(String testSuitePackage) {
        getTestsuites().getTestsuites().get(testSuiteIndex).setTestsuitePackage(testSuitePackage);
    }

    @Override
    public void setCurrentTestCaseName(String name) {
        getTestsuites().getTestsuites().get(testSuiteIndex).getTestcase().get(testCaseIndex).setName(name);
    }

    @Override
    public void setCurrentTestCaseFailure(String message, String type) {
        Error failure = new Error();
        failure.setMessage(message);
        failure.setType(type);
        getTestsuites().getTestsuites().get(testSuiteIndex).getTestcase().get(testCaseIndex).setFailure(failure);
    }

    @Override
    public void setCurrentTestCaseErrorMessage(String message, String type) {
        Error error = new Error();
        error.setMessage(message);
        error.setType(type);
        getTestsuites().getTestsuites().get(testSuiteIndex).getTestcase().get(testCaseIndex).setError(error);
    }
}

// Testsuites.java
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
class Testsuites {
    @XmlAttribute
    private String disabled;
    @XmlAttribute
    private String errors;
    @XmlAttribute
    private String failures;
    @XmlAttribute
    private String name;
    @XmlAttribute
    private String tests;
    @XmlAttribute
    private String time;

    private List<Testsuite> testsuite = new ArrayList<>();

    public List<Testsuite> getTestsuites() { return testsuite; }
    public void setTestsuite(List<Testsuite> value) { this.testsuite = value; }
    public void addTestSuite(Testsuite value) {this.testsuite.add(value);}

    public String getDisabled() { return disabled; }
    public void setDisabled(String value) { this.disabled = value; }

    public String getErrors() { return errors; }
    public void setErrors(String value) { this.errors = value; }

    public String getFailures() { return failures; }
    public void setFailures(String value) { this.failures = value; }

    public String getName() { return name; }
    public void setName(String value) { this.name = value; }

    public String getTests() { return tests; }
    public void setTests(String value) { this.tests = value; }

    public String getTime() { return time; }
    public void setTime(String value) { this.time = value; }
}

// Testsuite.java
@XmlAccessorType(XmlAccessType.FIELD)
class Testsuite {
    @XmlAttribute
    private String name;
    @XmlAttribute
    private String tests;
    @XmlAttribute
    private String disabled;
    @XmlAttribute
    private String errors;
    @XmlAttribute
    private String failures;
    @XmlAttribute
    private String hostname;
    @XmlAttribute(name = "id")
    private String testsuiteId;
    @XmlAttribute(name = "package")
    private String testsuitePackage;
    @XmlAttribute
    private String skipped;
    @XmlAttribute
    private String time;
    @XmlAttribute
    private String timestamp;

    private Properties properties;
    private List<Testcase> testcase = new ArrayList<>();

    @XmlElement(name = "system-out")
    private String systemOut;
    @XmlElement(name = "system-err")
    private String systemErr;

    public Properties getProperties() { return properties; }
    public void setProperties(Properties value) { this.properties = value; }

    public List<Testcase> getTestcase() { return testcase; }
    public void setTestcase(List<Testcase> value) { this.testcase = value; }
    public void addTestCase(Testcase value) {this.testcase.add(value);}

    public String getSystemOut() { return systemOut; }
    public void setSystemOut(String value) { this.systemOut = value; }

    public String getSystemErr() { return systemErr; }
    public void setSystemErr(String value) { this.systemErr = value; }

    public String getName() { return name; }
    public void setName(String value) { this.name = value; }

    public String getTests() { return tests; }
    public void setTests(String value) { this.tests = value; }

    public String getDisabled() { return disabled; }
    public void setDisabled(String value) { this.disabled = value; }

    public String getErrors() { return errors; }
    public void setErrors(String value) { this.errors = value; }

    public String getFailures() { return failures; }
    public void setFailures(String value) { this.failures = value; }

    public String getHostname() { return hostname; }
    public void setHostname(String value) { this.hostname = value; }

    public String getID() { return testsuiteId; }
    public void setID(String value) { this.testsuiteId = value; }

    public String getTestsuitePackage() { return testsuitePackage; }
    public void setTestsuitePackage(String value) { this.testsuitePackage = value; }

    public String getSkipped() { return skipped; }
    public void setSkipped(String value) { this.skipped = value; }

    public String getTime() { return time; }
    public void setTime(String value) { this.time = value; }

    public String getTimestamp() { return timestamp; }
    public void setTimestamp(String value) { this.timestamp = value; }
}

// Properties.java
@XmlAccessorType(XmlAccessType.FIELD)
class Properties {
    private List<Property> property = new ArrayList<>();

    public List<Property> getProperty() { return property; }
    public void setProperty(List<Property> value) { this.property = value; }
}

// Property.java
@XmlAccessorType(XmlAccessType.FIELD)
class Property {
    @XmlAttribute
    private String name;
    @XmlAttribute
    private String value;


    public String getName() { return name; }
    public void setName(String value) { this.name = value; }

    public String getValue() { return value; }
    public void setValue(String value) { this.value = value; }
}

// Testcase.java
@XmlAccessorType(XmlAccessType.FIELD)
class Testcase {
    @XmlAttribute
    private String name;
    @XmlAttribute
    private String assertions;
    @XmlAttribute
    private String classname;
    @XmlAttribute
    private String status;
    @XmlAttribute
    private String time;

    private Skipped skipped;
    private Error error;
    private Error failure;

    @XmlElement(name = "system-out")
    private String systemOut;
    @XmlElement(name = "system-err")
    private String systemErr;

    public Skipped getSkipped() { return skipped; }
    public void setSkipped(Skipped value) { this.skipped = value; }

    public Error getError() { return error; }
    public void setError(Error value) { this.error = value; }

    public Error getFailure() { return failure; }
    public void setFailure(Error value) { this.failure = value; }

    public String getSystemOut() { return systemOut; }
    public void setSystemOut(String value) { this.systemOut = value; }

    public String getSystemErr() { return systemErr; }
    public void setSystemErr(String value) { this.systemErr = value; }

    public String getName() { return name; }
    public void setName(String value) { this.name = value; }

    public String getAssertions() { return assertions; }
    public void setAssertions(String value) { this.assertions = value; }

    public String getClassname() { return classname; }
    public void setClassname(String value) { this.classname = value; }

    public String getStatus() { return status; }
    public void setStatus(String value) { this.status = value; }

    public String getTime() { return time; }
    public void setTime(String value) { this.time = value; }
}

// Error.java
@XmlAccessorType(XmlAccessType.FIELD)
class Error {
    @XmlAttribute
    private String message;
    @XmlAttribute
    private String type;
    @XmlValue
    private String text;


    public String getMessage() { return message; }
    public void setMessage(String value) { this.message = value; }

    public String getType() { return type; }
    public void setType(String value) { this.type = value; }

    public String getText() { return text; }
    public void setText(String value) { this.text = value; }
}

// Skipped.java
@XmlAccessorType(XmlAccessType.FIELD)
class Skipped {
    @XmlAttribute
    private String message;

    public String getMessage() { return message; }
    public void setMessage(String value) { this.message = value; }
}
