package org.dei.facts.parser;

/**
 * The ThenActionNode class represents an action node in the "then" block of a rule.
 * It contains the type of action and the value associated with the action.
 */
public class ThenActionNode extends ThenNode {
    String actionType; // The type of action (e.g., setEstado, setDiagnostico)
    String value; // The value associated with the action

    /**
     * Constructs a ThenActionNode with the specified action type and value.
     *
     * @param actionType the type of action
     * @param value the value associated with the action
     */
    public ThenActionNode(String actionType, String value) {
        this.actionType = actionType;
        this.value = value;
    }
}