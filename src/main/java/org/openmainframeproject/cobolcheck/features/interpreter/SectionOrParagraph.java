package org.openmainframeproject.cobolcheck.features.interpreter;

import java.util.ArrayList;
import java.util.List;

public class SectionOrParagraph {

    private List<String> lines;

    public SectionOrParagraph(){
        lines  = new ArrayList<>();
    }

    void addLine(String line){
        lines.add(line);
    }

    List<String> getLines(){
        return lines;
    }

    void removeLines(){ 
        lines.clear();
    }
}
