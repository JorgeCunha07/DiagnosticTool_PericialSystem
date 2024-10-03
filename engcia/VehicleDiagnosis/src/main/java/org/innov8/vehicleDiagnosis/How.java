package org.innov8.vehicleDiagnosis;

import org.innov8.model.Justification;

import java.util.Map;

import org.innov8.model.Fact;
import org.innov8.model.Hypothesis;

public class How {
    private Map<Integer, Justification> justifications;

    public How(Map<Integer, Justification> justifications) {
        this.justifications = justifications;
    }

    public String getHowExplanation(Integer factNumber) {
        return (getHowExplanation(factNumber, 0));
    }

    private String getHowExplanation(Integer factNumber, int level) {
        StringBuilder sb = new StringBuilder();
        Justification j = justifications.get(factNumber);
        if (j != null) { // justification for Fact factNumber was found
            sb.append(getIdentation(level));
            sb.append(j.getConclusion() + " was obtained by rule " + j.getRule() + " because");
            sb.append('\n');
            int l = level + 1;
            for (Fact f : j.getType1()) {
                sb.append(getIdentation(l));
                sb.append(f);
                sb.append('\n');
                if (f instanceof Hypothesis) {
                    String s = getHowExplanation(f.getId(), l + 1);
                    sb.append(s);
                }
            }
        }

        return sb.toString();
    }

    private String getIdentation(int level) {
        StringBuilder sb = new StringBuilder();
        for(int i=0; i < level; i++) {
            sb.append('\t');
        }
        return sb.toString();
    }
}
