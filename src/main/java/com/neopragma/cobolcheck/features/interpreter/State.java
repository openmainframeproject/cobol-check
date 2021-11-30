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
package com.neopragma.cobolcheck.features.interpreter;

import com.neopragma.cobolcheck.services.Constants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class State {
    private final Map<String, Flag> flags;

    public State() {
        flags = new HashMap<>();

        flags.put(Constants.IDENTIFICATION_DIVISION, new Flag());
        flags.put(Constants.ENVIRONMENT_DIVISION, new Flag());
        flags.put(Constants.CONFIGURATION_SECTION, new Flag());
        flags.put(Constants.INPUT_OUTPUT_SECTION, new Flag());
        flags.put(Constants.FILE_CONTROL, new Flag());
        flags.put(Constants.DATA_DIVISION, new Flag());
        flags.put(Constants.PROCEDURE_DIVISION, new Flag());
        flags.put(Constants.FILE_SECTION, new Flag());
        flags.put(Constants.LOCAL_STORAGE_SECTION, new Flag());
        flags.put(Constants.LINKAGE_SECTION, new Flag());
        flags.put(Constants.WORKING_STORAGE_SECTION, new Flag());

        flags.put(Constants.SELECT_TOKEN, new Flag());
        flags.put(Constants.FILE_STATUS_TOKEN, new Flag());
        flags.put(Constants.IS_TOKEN, new Flag());
        flags.put(Constants.FD_TOKEN, new Flag());
        flags.put(Constants.LEVEL_01_TOKEN, new Flag());
        flags.put(Constants.COPY_TOKEN, new Flag());
        flags.put(Constants.SECTION_TOKEN, new Flag());
        flags.put(Constants.PARAGRAPH_TOKEN, new Flag());

//IDENTIFICATION_DIVISION
        mutuallyExclusiveFlagsFor(Constants.IDENTIFICATION_DIVISION,
                Constants.DATA_DIVISION, Constants.ENVIRONMENT_DIVISION, Constants.PROCEDURE_DIVISION);

//ENVIRONMENT_DIVISION
        mutuallyExclusiveFlagsFor(Constants.ENVIRONMENT_DIVISION,
                Constants.IDENTIFICATION_DIVISION, Constants.DATA_DIVISION, Constants.PROCEDURE_DIVISION);
        dependentFlagsFor(Constants.ENVIRONMENT_DIVISION,
                Constants.CONFIGURATION_SECTION, Constants.INPUT_OUTPUT_SECTION);

//CONFIGURATION_SECTION
        mutuallyExclusiveFlagsFor(Constants.CONFIGURATION_SECTION,
                Constants.INPUT_OUTPUT_SECTION);

//INPUT_OUTPUT_SECTION
        mutuallyExclusiveFlagsFor(Constants.INPUT_OUTPUT_SECTION,
                Constants.CONFIGURATION_SECTION);
        dependentFlagsFor(Constants.INPUT_OUTPUT_SECTION,
                Constants.FILE_CONTROL);

//FILE_CONTROL


//SELECT_TOKEN
        dependentFlagsFor(Constants.SELECT_TOKEN,
                Constants.FILE_STATUS_TOKEN, Constants.IS_TOKEN);

//FILE_STATUS_TOKEN


//IS_TOKEN


//DATA_DIVISION
        mutuallyExclusiveFlagsFor(Constants.DATA_DIVISION,
                Constants.IDENTIFICATION_DIVISION, Constants.ENVIRONMENT_DIVISION, Constants.PROCEDURE_DIVISION);
        dependentFlagsFor(Constants.DATA_DIVISION,
                Constants.FILE_SECTION, Constants.LINKAGE_SECTION, Constants.LOCAL_STORAGE_SECTION, Constants.WORKING_STORAGE_SECTION);

//FILE_SECTION
        mutuallyExclusiveFlagsFor(Constants.FILE_SECTION,
                Constants.LINKAGE_SECTION, Constants.LOCAL_STORAGE_SECTION, Constants.WORKING_STORAGE_SECTION);
        dependentFlagsFor(Constants.FILE_SECTION,
                Constants.FD_TOKEN, Constants.LEVEL_01_TOKEN, Constants.COPY_TOKEN);

//FD_TOKEN
    dependentFlagsFor(Constants.FD_TOKEN,
            Constants.LEVEL_01_TOKEN, Constants.COPY_TOKEN);


//LEVEL_01_TOKEN


//COPY_TOKEN


//WORKING_STORAGE_SECTION
        mutuallyExclusiveFlagsFor(Constants.WORKING_STORAGE_SECTION,
                Constants.LINKAGE_SECTION, Constants.LOCAL_STORAGE_SECTION, Constants.FILE_SECTION);

//LOCAL_STORAGE_SECTION
        mutuallyExclusiveFlagsFor(Constants.LOCAL_STORAGE_SECTION,
                Constants.LINKAGE_SECTION, Constants.FILE_SECTION, Constants.WORKING_STORAGE_SECTION);

//LINKAGE_SECTION
        mutuallyExclusiveFlagsFor(Constants.LINKAGE_SECTION,
                Constants.FILE_SECTION, Constants.LOCAL_STORAGE_SECTION, Constants.WORKING_STORAGE_SECTION);

//PROCEDURE_DIVISION
        mutuallyExclusiveFlagsFor(Constants.PROCEDURE_DIVISION,
                Constants.IDENTIFICATION_DIVISION, Constants.ENVIRONMENT_DIVISION, Constants.DATA_DIVISION);

// SECTION
        dependentFlagsFor(Constants.SECTION_TOKEN,
                Constants.PARAGRAPH_TOKEN);
    }

    public Map<String, Flag> getFlags() {
        return flags;
    }

    public void setFlagFor(String key) { flags.get(key).set(); }
    public void unsetFlagFor(String key) { flags.get(key).unset(); }
    public boolean isFlagSetFor(String key) { return flags.get(key).isSet(); }

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

    public static class Flag {
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
                if (flag != null) flag.unset();
            }
        }
    }
}

