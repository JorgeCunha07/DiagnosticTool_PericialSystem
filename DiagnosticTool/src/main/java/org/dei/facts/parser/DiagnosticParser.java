package org.dei.facts.parser;

import java.io.*;
import java.util.*;
import java.util.regex.*;

public class DiagnosticParser {
    private Map<String, StateNode> stateNodes;

    public DiagnosticParser(Map<String, StateNode> stateNodes) {
        this.stateNodes = stateNodes;
    }

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
                    if (line.trim().equals("when")) {
                        inWhenSection = true;
                        inThenSection = false;
                        continue;
                    } else if (line.trim().equals("then")) {
                        inWhenSection = false;
                        inThenSection = true;
                        continue;
                    } else if (line.trim().equals("end")) {
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

    private void parseThenBlock(RuleInfo rule, List<String> thenBlockLines) {
        ThenNode root = parseThenNodes(new ArrayList<>(thenBlockLines).listIterator());
        rule.thenRootNode = root;
    }

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

    private void addRuleToGraph(RuleInfo rule) {
        if (rule.currentEstado == null) return;
        StateNode currentState = stateNodes.computeIfAbsent(rule.currentEstado, StateNode::new);
        processThenNode(currentState, rule.thenRootNode, rule.textoConditions, rule.ruleName);
    }

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

    public void traverseGraph(StateNode currentNode, List<String> path, List<DiagnosticPath> diagnosticPaths, Set<String> visited, String diagnosis, List<String> rules) {
        if (visited.contains(currentNode.getEstadoName())) {
            return; // Evita ciclos
        }

        visited.add(currentNode.getEstadoName());
        path.add(currentNode.getEstadoName());

        if (currentNode.isDiagnosisState() && diagnosis == null) {
            diagnosis = currentNode.getDiagnosis();
        }

        if ("finalizado".equals(currentNode.getEstadoName())) {
            if (diagnosis != null) {
                diagnosticPaths.add(new DiagnosticPath(diagnosis, new ArrayList<>(path), new ArrayList<>(rules)));
            }
        } else {
            for (Transition transition : currentNode.getTransitions()) {
                // Adiciona o nome da regra ao caminho das regras antes da transição
                List<String> newRules = new ArrayList<>(rules);
                newRules.add(transition.getRuleName());

                List<String> newPath = new ArrayList<>(path);
                Set<String> newVisited = new HashSet<>(visited);

                traverseGraph(transition.getTargetState(), newPath, diagnosticPaths, newVisited, diagnosis, newRules);
            }
        }

        path.remove(path.size() - 1);
        visited.remove(currentNode.getEstadoName());
    }

}
