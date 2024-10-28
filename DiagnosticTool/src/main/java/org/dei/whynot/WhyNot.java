package org.dei.whynot;

import java.util.List;
import java.util.stream.Collectors;

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
        List<String> triggeredRules = droolsWithWhyNot.getTriggeredRules();

        List<String> relevantUntriggeredRules = untriggeredRules.stream()
                .filter(rule -> rule.contains(expectedConclusion))
                .collect(Collectors.toList());

        if (!relevantUntriggeredRules.isEmpty()) {
            StringBuilder explanation = new StringBuilder();
            explanation.append("A conclusão '").append(expectedConclusion).append("' não foi atingida porque as seguintes regras não foram disparadas:\n");
            for (String rule : relevantUntriggeredRules) {
                explanation.append("- ").append(rule).append(": Algumas condições não foram atendidas.\n");
            }
            return explanation.toString();
        }

        return "Todas as regras necessárias para a conclusão '" + expectedConclusion + "' foram disparadas.";
    }
}