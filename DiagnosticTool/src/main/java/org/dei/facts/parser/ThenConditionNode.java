package org.dei.facts.parser;

/**
 * The ThenConditionNode class represents a condition node in the "then" block of a rule.
 * It contains the condition and the subsequent ThenNode to execute if the condition is met.
 */
public class ThenConditionNode extends ThenNode {
    String condition; // The condition to be evaluated
    ThenNode thenNode; // The subsequent ThenNode to execute if the condition is met

    /**
     * Constructs a ThenConditionNode with the specified condition.
     *
     * @param condition the condition to be evaluated
     */
    public ThenConditionNode(String condition) {
        this.condition = condition;
    }
}