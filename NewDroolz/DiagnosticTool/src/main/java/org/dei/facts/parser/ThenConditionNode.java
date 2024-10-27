package org.dei.facts.parser;

public class ThenConditionNode extends ThenNode {
    String condition;
    ThenNode thenNode;

    public ThenConditionNode(String condition) {
        this.condition = condition;
    }
}
