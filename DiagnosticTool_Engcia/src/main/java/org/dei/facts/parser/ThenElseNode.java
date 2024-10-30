package org.dei.facts.parser;

/**
 * The ThenElseNode class represents an "else" node in the "then" block of a rule.
 * It contains a reference to the subsequent ThenNode to execute if the "else" condition is met.
 */
public class ThenElseNode extends ThenNode {
    ThenNode thenNode; // The subsequent ThenNode to execute if the "else" condition is met
}