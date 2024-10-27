import java.io.*;
import java.util.*;
import java.util.regex.*;

public class DiagnosticParser {

    public static void main(String[] args) {
        // Mapa para armazenar estados e transições
        Map<String, StateNode> stateNodes = new HashMap<>();
        // Lista para armazenar todos os diagnósticos e seus caminhos
        List<DiagnosticPath> diagnosticPaths = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(new FileReader("diagnostic.drl"))) {
            System.out.println("Arquivo diagnostic.drl encontrado e sendo lido...");
            String line;
            RuleInfo currentRule = null;
            boolean inWhenSection = false;
            boolean inThenSection = false;
            List<String> thenBlockLines = new ArrayList<>();

            while ((line = reader.readLine()) != null) {
                // Detecta o início de uma nova regra
                Matcher ruleMatcher = Pattern.compile("rule\\s+\"(.+?)\"").matcher(line);
                if (ruleMatcher.find()) {
                    // Início de uma nova regra
                    currentRule = new RuleInfo();
                    currentRule.ruleName = ruleMatcher.group(1);
                    inWhenSection = false;
                    inThenSection = false;
                    thenBlockLines.clear();
                    continue;
                }

                if (currentRule != null) {
                    // Verifica as seções 'when' e 'then'
                    if (line.trim().equals("when")) {
                        inWhenSection = true;
                        inThenSection = false;
                        continue;
                    } else if (line.trim().equals("then")) {
                        inWhenSection = false;
                        inThenSection = true;
                        continue;
                    } else if (line.trim().equals("end")) {
                        // Processa as informações coletadas
                        parseThenBlock(currentRule, thenBlockLines);
                        // Constrói o grafo
                        addRuleToGraph(currentRule, stateNodes);
                        currentRule = null;
                        continue;
                    }

                    if (inWhenSection) {
                        // Extrai condições
                        // Para a condição 'estado'
                        Matcher estadoMatcher = Pattern.compile("\\$resposta\\s*:\\s*Resposta\\s*\\(\\s*estado\\s*==\\s*\"(.*?)\"").matcher(line);
                        if (estadoMatcher.find()) {
                            currentRule.currentEstado = estadoMatcher.group(1);
                            // Debugging
                            System.out.println("Regra " + currentRule.ruleName + ": Estado atual capturado: " + currentRule.currentEstado);
                        }
                        // Captura todas as condições de 'texto'
                        Matcher textoConditionMatcher = Pattern.compile("(texto\\s*(==|!=|equals|equalsIgnoreCase)\\s*(\".*?\"|null))").matcher(line);
                        while (textoConditionMatcher.find()) {
                            String condition = textoConditionMatcher.group(1).trim();
                            currentRule.textoConditions.add(condition);
                            // Debugging
                            System.out.println("Regra " + currentRule.ruleName + ": Condição 'texto' capturada: " + condition);
                        }
                    } else if (inThenSection) {
                        thenBlockLines.add(line);
                    }
                }
            }

            // Verifica se o estado inicial existe
            StateNode initialState = stateNodes.get("iniciarDiagnostico"); // Ajuste se o estado inicial for diferente
            if (initialState == null) {
                System.out.println("Estado inicial 'iniciarDiagnostico' não encontrado. Estados disponíveis:");
                for (String stateName : stateNodes.keySet()) {
                    System.out.println("  " + stateName);
                }
                return;
            }

            // Percorre o grafo para encontrar todos os possíveis caminhos para diagnósticos
            List<String> path = new ArrayList<>();
            Set<String> visited = new HashSet<>();
            traverseGraph(initialState, path, diagnosticPaths, visited);

            // Imprime os caminhos de diagnóstico
            System.out.println("\nPossíveis caminhos de diagnóstico:");
            for (DiagnosticPath dp : diagnosticPaths) {
                System.out.println("Diagnóstico: " + dp.diagnosis);
                System.out.println("Caminho completo: " + dp.path);
                System.out.println();
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void parseThenBlock(RuleInfo rule, List<String> thenBlockLines) {
        // Parser para construir uma árvore de ThenNodes
        ListIterator<String> iterator = thenBlockLines.listIterator();
        ThenNode root = parseThenNodes(iterator);
        rule.thenRootNode = root;
    }

    private static ThenNode parseThenNodes(ListIterator<String> iterator) {
        ThenSequenceNode sequenceNode = new ThenSequenceNode();
        while (iterator.hasNext()) {
            String line = iterator.next().trim();

            // Ignora linhas vazias ou comentários
            if (line.isEmpty() || line.startsWith("//")) {
                continue;
            }

            // Verifica declarações 'if'
            Matcher ifMatcher = Pattern.compile("if\\s*\\((.*?)\\)\\s*\\{?").matcher(line);
            if (ifMatcher.find()) {
                String condition = ifMatcher.group(1).trim();
                ThenConditionNode conditionNode = new ThenConditionNode(condition);
                // Parse the body inside the 'if'
                conditionNode.thenNode = parseThenNodes(iterator);
                sequenceNode.nodes.add(conditionNode);
                continue;
            }

            // Verifica declarações 'else if'
            Matcher elseIfMatcher = Pattern.compile("else\\s+if\\s*\\((.*?)\\)\\s*\\{?").matcher(line);
            if (elseIfMatcher.find()) {
                String condition = elseIfMatcher.group(1).trim();
                ThenConditionNode conditionNode = new ThenConditionNode(condition);
                // Parse the body inside the 'else if'
                conditionNode.thenNode = parseThenNodes(iterator);
                sequenceNode.nodes.add(conditionNode);
                continue;
            }

            // Verifica declarações 'else'
            Matcher elseMatcher = Pattern.compile("else\\s*\\{?").matcher(line);
            if (elseMatcher.find()) {
                ThenElseNode elseNode = new ThenElseNode();
                // Parse the body inside the 'else'
                elseNode.thenNode = parseThenNodes(iterator);
                sequenceNode.nodes.add(elseNode);
                continue;
            }

            // Verifica fim de bloco '}'
            if (line.equals("}")) {
                // Fim do bloco atual
                break;
            }

            // Verifica ações
            Matcher setEstadoMatcher = Pattern.compile("\\$resposta\\.setEstado\\(\"(.*?)\"\\);").matcher(line);
            if (setEstadoMatcher.find()) {
                String nextEstado = setEstadoMatcher.group(1);
                ThenActionNode actionNode = new ThenActionNode("setEstado", nextEstado);
                sequenceNode.nodes.add(actionNode);
                continue;
            }

            Matcher setDiagnosticoMatcher = Pattern.compile("\\$resposta\\.setDiagnostico\\(\"(.*?)\"\\);").matcher(line);
            if (setDiagnosticoMatcher.find()) {
                String diagnosis = setDiagnosticoMatcher.group(1);
                ThenActionNode actionNode = new ThenActionNode("setDiagnostico", diagnosis);
                sequenceNode.nodes.add(actionNode);
                continue;
            }

            // Ignora outras linhas ou pode adicionar suporte para outras ações
        }
        return sequenceNode;
    }

    private static void addRuleToGraph(RuleInfo rule, Map<String, StateNode> stateNodes) {
        // Garante que o nó de estado atual exista
        if (rule.currentEstado == null) {
            // Se não houver estado atual, ignora a regra
            return;
        }

        StateNode currentState = stateNodes.computeIfAbsent(rule.currentEstado, k -> new StateNode(k));
        // Cria transições a partir do thenRootNode
        processThenNode(currentState, rule.thenRootNode, rule.textoConditions, rule.ruleName, stateNodes);
    }

    private static void processThenNode(StateNode currentState, ThenNode thenNode, List<String> parentConditions, String ruleName, Map<String, StateNode> stateNodes) {
        if (thenNode instanceof ThenSequenceNode) {
            ThenSequenceNode seqNode = (ThenSequenceNode) thenNode;
            for (ThenNode childNode : seqNode.nodes) {
                processThenNode(currentState, childNode, parentConditions, ruleName, stateNodes);
            }
        } else if (thenNode instanceof ThenActionNode) {
            ThenActionNode actionNode = (ThenActionNode) thenNode;
            if (actionNode.actionType.equals("setEstado")) {
                // Adiciona uma transição para o próximo estado
                StateNode nextState = stateNodes.computeIfAbsent(actionNode.value, k -> new StateNode(k));
                Transition transition = new Transition(parentConditions, nextState, ruleName);
                currentState.transitions.add(transition);
                // Debugging
                System.out.println("Transição adicionada do estado " + currentState.estadoName + " para " + nextState.estadoName + " via regra " + ruleName + " com condições " + parentConditions);
            } else if (actionNode.actionType.equals("setDiagnostico")) {
                // Marca o estado atual como estado de diagnóstico
                currentState.isDiagnosisState = true;
                currentState.diagnosis = actionNode.value;
                // Debugging
                System.out.println("Estado " + currentState.estadoName + " marcado como diagnóstico: " + currentState.diagnosis);
            }
        } else if (thenNode instanceof ThenConditionNode) {
            ThenConditionNode condNode = (ThenConditionNode) thenNode;
            List<String> combinedConditions = new ArrayList<>(parentConditions);
            combinedConditions.add(condNode.condition);
            processThenNode(currentState, condNode.thenNode, combinedConditions, ruleName, stateNodes);
        } else if (thenNode instanceof ThenElseNode) {
            ThenElseNode elseNode = new ThenElseNode();
            List<String> elseConditions = new ArrayList<>(parentConditions);
            // Negar a última condição
            if (!elseConditions.isEmpty()) {
                String lastCondition = elseConditions.remove(elseConditions.size() - 1);
                elseConditions.add(negateCondition(lastCondition));
            }
            processThenNode(currentState, elseNode.thenNode, elseConditions, ruleName, stateNodes);
        }
    }

    private static String negateCondition(String condition) {
        return "!(" + condition + ")";
    }

    private static void traverseGraph(StateNode currentNode, List<String> path, List<DiagnosticPath> diagnosticPaths, Set<String> visited) {
        if (visited.contains(currentNode.estadoName)) {
            // Evita ciclos
            return;
        }
        visited.add(currentNode.estadoName);
        path.add(currentNode.estadoName);

        if (currentNode.isDiagnosisState) {
            // Chegou a um diagnóstico
            DiagnosticPath dp = new DiagnosticPath(currentNode.diagnosis, new ArrayList<>(path));
            diagnosticPaths.add(dp);
        }

        if (!currentNode.transitions.isEmpty()) {
            // Continua percorrendo
            for (Transition transition : currentNode.transitions) {
                // Simula cada possível condição para o próximo estado
                List<String> newPath = new ArrayList<>(path);
                newPath.add(transition.ruleName + " " + transition.conditions);
                Set<String> newVisited = new HashSet<>(visited);
                traverseGraph(transition.targetState, newPath, diagnosticPaths, newVisited);
            }
        }

        path.remove(path.size() - 1);
        visited.remove(currentNode.estadoName);
    }

    // Classes auxiliares
    static class StateNode {
        String estadoName;
        List<Transition> transitions = new ArrayList<>();
        boolean isDiagnosisState = false;
        String diagnosis;

        StateNode(String estadoName) {
            this.estadoName = estadoName;
        }
    }

    static class Transition {
        List<String> conditions; // Lista de condições
        StateNode targetState;
        String ruleName;

        Transition(List<String> conditions, StateNode targetState, String ruleName) {
            this.conditions = conditions;
            this.targetState = targetState;
            this.ruleName = ruleName;
        }
    }

    static class RuleInfo {
        String ruleName;
        String currentEstado;
        List<String> textoConditions = new ArrayList<>(); // Lista de condições 'texto'
        ThenNode thenRootNode;
    }

    static abstract class ThenNode {
    }

    static class ThenSequenceNode extends ThenNode {
        List<ThenNode> nodes = new ArrayList<>();
    }

    static class ThenActionNode extends ThenNode {
        String actionType; // 'setEstado' ou 'setDiagnostico'
        String value;

        ThenActionNode(String actionType, String value) {
            this.actionType = actionType;
            this.value = value;
        }
    }

    static class ThenConditionNode extends ThenNode {
        String condition;
        ThenNode thenNode;

        ThenConditionNode(String condition) {
            this.condition = condition;
        }
    }

    static class ThenElseNode extends ThenNode {
        ThenNode thenNode;
    }

    static class DiagnosticPath {
        String diagnosis;
        List<String> path;

        DiagnosticPath(String diagnosis, List<String> path) {
            this.diagnosis = diagnosis;
            this.path = path;
        }
    }
}
