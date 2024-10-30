package org.dei.facts.parser;

import java.util.*;

/**
 * The RuleInfo class represents information about a diagnostic rule, including its name,
 * current state, text conditions, and the root node of the "then" block.
 */
public class RuleInfo {
    String ruleName; // The name of the rule
    String currentEstado; // The current state associated with the rule
    List<String> textoConditions = new ArrayList<>(); // The list of text conditions for the rule
    ThenNode thenRootNode; // The root node of the "then" block

    /**
     * Constructs a RuleInfo object with the specified rule name.
     *
     * @param ruleName the name of the rule
     */
    public RuleInfo(String ruleName) {
        this.ruleName = ruleName;
    }
}