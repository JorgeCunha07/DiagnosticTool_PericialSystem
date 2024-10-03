package org.innov8.model;

import java.util.ArrayList;
import java.util.List;

public class Justification {
    private String rule;
    private List<Fact> type1;
    private Fact conclusion;

    public Justification(String rule, List<Fact> type1, Fact conclusion) {
        this.rule = rule;
        this.type1 = new ArrayList<Fact>(type1);
        this.conclusion = conclusion;
    }


    public String getRule() {
        return this.rule;
    }

    public List<Fact> getType1() {
        return this.type1;
    }

    public Fact getConclusion() {
        return this.conclusion;
    }

}
