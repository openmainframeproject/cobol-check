package com.neopragma.cobolcheck.structure;

import java.util.ArrayList;
import java.util.List;

public class CobolStructure {

    private final List<CobolDivision> divisions;

    public CobolStructure() {
        divisions = new ArrayList();
        divisions.add(new IdentificationDivision());
//        divisions.add(new EnvironmentDivision());
//        divisions.add(new DataDivision());
//        divisions.add(new ProcedureDivision());
    }
}
