package org.dei.facts.parser;

import java.util.ArrayList;
import java.util.List;

/**
 * The ThenSequenceNode class represents a sequence of ThenNode objects in the "then" block of a rule.
 * It contains a list of ThenNode objects that are executed in sequence.
 */
public class ThenSequenceNode extends ThenNode {
    List<ThenNode> nodes = new ArrayList<>(); // The list of ThenNode objects to be executed in sequence
}