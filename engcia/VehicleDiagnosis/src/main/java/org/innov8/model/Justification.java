package org.innov8.model;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public class Justification {
    private String rule;
    private List<Fact> lhs;
    private Fact conclusion;

    public Justification(String rule, List<Fact> lhs, Fact conclusion) {
        this.rule = rule;
        this.lhs = new ArrayList<Fact>(lhs);
        this.conclusion = conclusion;
    }

}
