/*
Copyright 2020 David Nicolette

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.neopragma.cobolcheck;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class State implements Constants {
    private final Map<String, Flag> flags;

    public State() {
        flags = new HashMap<>();

        flags.put(IDENTIFICATION_DIVISION, new Flag());

        flags.put(FILE_SECTION, new Flag());
        flags.put(LINKAGE_SECTION, new Flag());
        flags.put(LOCAL_STORAGE_SECTION, new Flag());
        flags.put(WORKING_STORAGE_SECTION, new Flag());
        mutuallyExclusiveFlagsFor(FILE_SECTION,
                LINKAGE_SECTION, LOCAL_STORAGE_SECTION, WORKING_STORAGE_SECTION);
        mutuallyExclusiveFlagsFor(LINKAGE_SECTION,
                FILE_SECTION, LOCAL_STORAGE_SECTION, WORKING_STORAGE_SECTION);
        mutuallyExclusiveFlagsFor(LOCAL_STORAGE_SECTION,
                LINKAGE_SECTION, FILE_SECTION, WORKING_STORAGE_SECTION);
        mutuallyExclusiveFlagsFor(WORKING_STORAGE_SECTION,
                LINKAGE_SECTION, LOCAL_STORAGE_SECTION, FILE_SECTION);

        flags.put(CONFIGURATION_SECTION, new Flag());
        flags.put(FILE_CONTROL, new Flag());
        flags.put(INPUT_OUTPUT_SECTION, new Flag());
        dependentFlagsFor(INPUT_OUTPUT_SECTION,
                FILE_CONTROL);
        mutuallyExclusiveFlagsFor(CONFIGURATION_SECTION,
                INPUT_OUTPUT_SECTION);
        mutuallyExclusiveFlagsFor(INPUT_OUTPUT_SECTION,
                CONFIGURATION_SECTION);

        flags.put(ENVIRONMENT_DIVISION, new Flag());
        dependentFlagsFor(ENVIRONMENT_DIVISION,
                CONFIGURATION_SECTION, INPUT_OUTPUT_SECTION);

        flags.put(DATA_DIVISION, new Flag());
        dependentFlagsFor(DATA_DIVISION,
                FILE_SECTION, LINKAGE_SECTION, LOCAL_STORAGE_SECTION, WORKING_STORAGE_SECTION);

        flags.put(PROCEDURE_DIVISION, new Flag());

        mutuallyExclusiveFlagsFor(IDENTIFICATION_DIVISION,
                DATA_DIVISION, ENVIRONMENT_DIVISION, PROCEDURE_DIVISION);
        mutuallyExclusiveFlagsFor(ENVIRONMENT_DIVISION,
                IDENTIFICATION_DIVISION, DATA_DIVISION, PROCEDURE_DIVISION);
        mutuallyExclusiveFlagsFor(DATA_DIVISION,
                IDENTIFICATION_DIVISION, ENVIRONMENT_DIVISION, PROCEDURE_DIVISION);
        mutuallyExclusiveFlagsFor(PROCEDURE_DIVISION,
                IDENTIFICATION_DIVISION, ENVIRONMENT_DIVISION, DATA_DIVISION);
    }

    Map<String, Flag> getFlags() {
        return flags;
    }

    private void mutuallyExclusiveFlagsFor(String token, String... mutuallyExclusiveTokens) {
        List<Flag> mutuallyExclusiveFlags = new ArrayList<>();
        for (String mutuallyExclusiveToken : mutuallyExclusiveTokens) {
            mutuallyExclusiveFlags.add(flags.get(mutuallyExclusiveToken));
        }
        flags.get(token).setMutuallyExclusiveFlags(mutuallyExclusiveFlags);
    }

    private void dependentFlagsFor(String token, String... dependentTokens) {
        List<Flag> dependentFlags = new ArrayList<>();
        for (String dependentToken : dependentTokens) {
            dependentFlags.add(flags.get(dependentToken));
        }
        flags.get(token).setDependentFlags(dependentFlags);
    }

    static class Flag {
        private boolean state = false;
        private List<Flag> mutuallyExclusiveFlags;
        private List<Flag> dependentFlags;

        public Flag() {
            this.mutuallyExclusiveFlags = new ArrayList<>();
            this.dependentFlags = new ArrayList<>();
        }

        public void setMutuallyExclusiveFlags(List<Flag> mutuallyExclusiveFlags) {
            this.mutuallyExclusiveFlags = mutuallyExclusiveFlags;
        }

        public void setDependentFlags(List<Flag> dependentFlags) {
            this.dependentFlags = dependentFlags;
        }

        public boolean isSet() {
            return state;
        }

        public void set() {
            state = true;
            for (Flag flag : mutuallyExclusiveFlags) {
                flag.unset();
            }
        }

        public void unset() {
            state = false;
            for (Flag flag : dependentFlags) {
                flag.unset();
            }
        }
    }
}

