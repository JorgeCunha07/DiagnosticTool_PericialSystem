package org.dei.facts.parser;

import java.io.*;
import java.util.*;
import java.util.regex.*;

/**
 * The DiagnosticParser class is responsible for parsing diagnostic rules from a file
 * and constructing a state graph based on the parsed rules.
 */
public class DiagnosticParser {
    private Map<String, StateNode> stateNodes; // A map of state names to StateNode objects

    /**
     * Constructs a DiagnosticParser with the specified state nodes.
     *
     * @param stateNodes a map of state names to StateNode objects
     */
    public DiagnosticParser(Map<String, StateNode> stateNodes) {
        this.stateNodes = stateNodes;
    }

    /**
     * Parses a file containing diagnostic rules and constructs the state graph.
     *
     * @param filePath the path to the file containing diagnostic rules
     */
    public void parseFile(String filePath) {
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            RuleInfo currentRule = null;
            boolean inWhenSection = false;
            boolean inThenSection = false;
            List<String> thenBlockLines = new ArrayList<>();

            while ((line = reader.readLine()) != null) {
                Matcher ruleMatcher = Pattern.compile("rule\\s+\"(.+?)\"").matcher(line);
                if (ruleMatcher.find()) {
                    currentRule = new RuleInfo(ruleMatcher.group(1));
                    inWhenSection = false;
                    inThenSection = false;
                    thenBlockLines.clear();
                    continue;
                }

                if (currentRule != null) {
                    switch (line.trim()) {
                        case "when":
                            inWhenSection = true;
                            inThenSection = false;
                            continue;
                        case "then":
                            inWhenSection = false;
                            inThenSection = true;
                            continue;
                        case "end":
                            parseThenBlock(currentRule, thenBlockLines);
                            addRuleToGraph(currentRule);
                            currentRule = null;
                            continue;
                    }

                    if (inWhenSection) {
                        parseWhenSection(line, currentRule);
                    } else if (inThenSection) {
                        thenBlockLines.add(line);
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Parses the "when" section of a rule and updates the RuleInfo object.
     *
     * @param line the line of text in the "when" section
     * @param rule the RuleInfo object to update
     */
    private void parseWhenSection(String line, RuleInfo rule) {
        Matcher estadoMatcher = Pattern.compile("\\$resposta\\s*:\\s*Resposta\\s*\\(\\s*estado\\s*==\\s*\"(.*?)\"").matcher(line);
        if (estadoMatcher.find()) {
            rule.currentEstado = estadoMatcher.group(1);
        }

        Matcher textoConditionMatcher = Pattern.compile("(texto\\s*(==|!=|equals|equalsIgnoreCase)\\s*(\".*?\"|null))").matcher(line);
        while (textoConditionMatcher.find()) {
            rule.textoConditions.add(textoConditionMatcher.group(1).trim());
        }
    }

    /**
     * Parses the "then" block of a rule and updates the RuleInfo object.
     *
     * @param rule the RuleInfo object to update
     * @param thenBlockLines the lines of text in the "then" block
     */
    private void parseThenBlock(RuleInfo rule, List<String> thenBlockLines) {
        ThenNode root = parseThenNodes(new ArrayList<>(thenBlockLines).listIterator());
        rule.thenRootNode = root;
    }

    /**
     * Parses the nodes in the "then" block and constructs a ThenNode tree.
     *
     * @param iterator an iterator over the lines in the "then" block
     * @return the root ThenNode of the parsed tree
     */
    private ThenNode parseThenNodes(ListIterator<String> iterator) {
        ThenSequenceNode sequenceNode = new ThenSequenceNode();
        while (iterator.hasNext()) {
            String line = iterator.next().trim();
            if (line.isEmpty() || line.startsWith("//")) continue;
            Matcher ifMatcher = Pattern.compile("if\\s*\\((.*?)\\)\\s*\\{?").matcher(line);
            if (ifMatcher.find()) {
                ThenConditionNode conditionNode = new ThenConditionNode(ifMatcher.group(1).trim());
                conditionNode.thenNode = parseThenNodes(iterator);
                sequenceNode.nodes.add(conditionNode);
                continue;
            }
            Matcher setEstadoMatcher = Pattern.compile("\\$resposta\\.setEstado\\(\"(.*?)\"\\);").matcher(line);
            if (setEstadoMatcher.find()) {
                sequenceNode.nodes.add(new ThenActionNode("setEstado", setEstadoMatcher.group(1)));
                continue;
            }
            Matcher setDiagnosticoMatcher = Pattern.compile("\\$resposta\\.setDiagnostico\\(\"(.*?)\"\\);").matcher(line);
            if (setDiagnosticoMatcher.find()) {
                sequenceNode.nodes.add(new ThenActionNode("setDiagnostico", setDiagnosticoMatcher.group(1)));
                continue;
            }
            if (line.equals("}")) break;
        }
        return sequenceNode;
    }

    /**
     * Adds a parsed rule to the state graph.
     *
     * @param rule the RuleInfo object representing the parsed rule
     */
    private void addRuleToGraph(RuleInfo rule) {
        if (rule.currentEstado == null) return;
        StateNode currentState = stateNodes.computeIfAbsent(rule.currentEstado, StateNode::new);
        processThenNode(currentState, rule.thenRootNode, rule.textoConditions, rule.ruleName);
    }

    /**
     * Processes a ThenNode and updates the state graph accordingly.
     *
     * @param currentState the current StateNode
     * @param thenNode the ThenNode to process
     * @param parentConditions the conditions from the parent nodes
     * @param ruleName the name of the rule
     */
    private void processThenNode(StateNode currentState, ThenNode thenNode, List<String> parentConditions, String ruleName) {
        if (thenNode instanceof ThenSequenceNode) {
            for (ThenNode childNode : ((ThenSequenceNode) thenNode).nodes) {
                processThenNode(currentState, childNode, parentConditions, ruleName);
            }
        } else if (thenNode instanceof ThenActionNode) {
            ThenActionNode actionNode = (ThenActionNode) thenNode;
            if (actionNode.actionType.equals("setEstado")) {
                StateNode nextState = stateNodes.computeIfAbsent(actionNode.value, StateNode::new);
                currentState.addTransition(new Transition(parentConditions, nextState, ruleName));
            } else if (actionNode.actionType.equals("setDiagnostico")) {
                currentState.setDiagnosis(actionNode.value);
            }
        } else if (thenNode instanceof ThenConditionNode) {
            ThenConditionNode condNode = (ThenConditionNode) thenNode;
            List<String> combinedConditions = new ArrayList<>(parentConditions);
            combinedConditions.add(condNode.condition);
            processThenNode(currentState, condNode.thenNode, combinedConditions, ruleName);
        }
    }

    /**
     * Traverses the state graph starting from the given node and collects diagnostic paths.
     *
     * @param currentNode the starting StateNode
     * @param path the current path of state names
     * @param diagnosticPaths the list to collect diagnostic paths
     * @param visited the set of visited state names to avoid cycles
     */
    public void traverseGraph(StateNode currentNode, List<String> path, List<DiagnosticPath> diagnosticPaths, Set<String> visited) {
        traverseGraphHelper(currentNode, path, diagnosticPaths, visited, new ArrayList<>(), null);
    }

    /**
     * Helper method to traverse the state graph and collect diagnostic paths.
     *
     * @param currentNode the current StateNode
     * @param path the current path of state names
     * @param diagnosticPaths the list to collect diagnostic paths
     * @param visited the set of visited state names to avoid cycles
     * @param rules the list of rules applied along the path
     * @param diagnosis the current diagnosis
     */
    private void traverseGraphHelper(StateNode currentNode, List<String> path, List<DiagnosticPath> diagnosticPaths, Set<String> visited, List<String> rules, String diagnosis) {
        if (visited.contains(currentNode.getEstadoName())) return; // Avoid cycles

        path.add(currentNode.getEstadoName());
        visited.add(currentNode.getEstadoName());

        // Update diagnosis if in a diagnosis state
        if (currentNode.isDiagnosisState()) {
            diagnosis = currentNode.getDiagnosis();
        }

        // If reached final state, add to diagnostic paths if diagnosis is present
        if ("finalizado".equals(currentNode.getEstadoName()) && diagnosis != null) {
            diagnosticPaths.add(new DiagnosticPath(diagnosis, new ArrayList<>(path), new ArrayList<>(rules)));
        } else {
            for (Transition transition : currentNode.getTransitions()) {
                List<String> newRules = new ArrayList<>(rules);
                newRules.add(transition.getRuleName());

                traverseGraphHelper(transition.getTargetState(), new ArrayList<>(path), diagnosticPaths, new HashSet<>(visited), newRules, diagnosis);
            }
        }

        path.remove(path.size() - 1);
        visited.remove(currentNode.getEstadoName());
    }
}