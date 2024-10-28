package org.dei.facts.parser;

import java.util.*;

public class RuleInfo {
    String ruleName;
    String currentEstado;
    List<String> textoConditions = new ArrayList<>();
    ThenNode thenRootNode;

    public RuleInfo(String ruleName) {
        this.ruleName = ruleName;
    }
}
