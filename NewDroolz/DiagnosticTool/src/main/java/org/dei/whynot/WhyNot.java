package org.dei.whynot;

import java.util.List;

public class WhyNot {
    private static WhyNot instance;
    private DroolsWithWhyNot droolsWithWhyNot;

    public WhyNot(DroolsWithWhyNot droolsWithWhyNot) {
        this.droolsWithWhyNot = droolsWithWhyNot;
    }

    public synchronized static WhyNot init(DroolsWithWhyNot droolsWithWhyNot) {
        if (instance == null) {
            instance = new WhyNot(droolsWithWhyNot);
        }
        return instance;
    }

    public String getWhyNotExplanation(String expectedConclusion) {
        List<String> untriggeredRules = droolsWithWhyNot.getUntriggeredRules();

        // Check if the expected conclusion's rule is in the list of untriggered rules
        if (untriggeredRules.contains(expectedConclusion)) {
            return "The rule '" + expectedConclusion + "' did not fire because some conditions were not met.";
        }
        return "The rule '" + expectedConclusion + "' was not found in the list of untriggered rules.";
    }


}
