package org.openmainframeproject.cobolcheck.features.interpreter;

import java.util.ArrayList;
import java.util.List;

public class Block {

    private List<String> lines;

    public Block(){
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
