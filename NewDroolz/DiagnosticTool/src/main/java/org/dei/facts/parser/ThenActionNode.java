package org.dei.facts.parser;

public class ThenActionNode extends ThenNode {
    String actionType;
    String value;

    public ThenActionNode(String actionType, String value) {
        this.actionType = actionType;
        this.value = value;
    }
}

