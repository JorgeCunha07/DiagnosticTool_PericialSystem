package org.dei.facts.parser;

import lombok.Getter;

import java.util.*;

/**
 * The Transition class represents a transition in a state machine.
 * It contains the conditions that trigger the transition, the target state,
 * and the name of the rule associated with the transition.
 */
@Getter
public class Transition {
    private List<String> conditions; // The conditions that trigger the transition
    private StateNode targetState; // The target state of the transition
    private String ruleName; // The name of the rule associated with the transition

    /**
     * Constructs a Transition with the specified conditions, target state, and rule name.
     *
     * @param conditions the conditions that trigger the transition
     * @param targetState the target state of the transition
     * @param ruleName the name of the rule associated with the transition
     */
    public Transition(List<String> conditions, StateNode targetState, String ruleName) {
        this.conditions = conditions;
        this.targetState = targetState;
        this.ruleName = ruleName;
    }
}