package org.dei.facts.parser;

import lombok.Getter;

import java.util.*;

@Getter
public class Transition {
    private List<String> conditions;
    private StateNode targetState;
    private String ruleName;

    public Transition(List<String> conditions, StateNode targetState, String ruleName) {
        this.conditions = conditions;
        this.targetState = targetState;
        this.ruleName = ruleName;
    }

}
