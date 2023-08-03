package org.openmainframeproject.cobolcheck.features.interpreter;

import java.util.ArrayList;
import java.util.List;

public class MockedSectionOrPargraph {

    private List<String> sectionLines;

    public MockedSectionOrPargraph(){
        sectionLines  = new ArrayList<>();
    }

    void addSectionLine(String line){
        sectionLines.add(line);
    }

    List<String> getSectionLines(){
        return sectionLines;
    }

    void removeSectionLines(){ 
        sectionLines.clear();
    }
}
