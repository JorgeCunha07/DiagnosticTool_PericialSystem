package org.dei.service;

import lombok.Getter;
import org.dei.facts.parser.DiagnosticParser;
import org.dei.facts.parser.DiagnosticPath;
import org.dei.facts.parser.StateNode;
import org.springframework.stereotype.Service;

import java.util.*;

@Getter
@Service
public class DiagnosticParserService {
    private Map<String, StateNode> stateNodes = new HashMap<>();
    private List<DiagnosticPath> diagnosticPaths = new ArrayList<>();

    public void processDiagnosticFile(String filePath) {
        DiagnosticParser parser = new DiagnosticParser(stateNodes);
        parser.parseFile(filePath);

        StateNode initialState = stateNodes.get("iniciarDiagnostico");
        if (initialState == null) {
            return;
        }

        List<String> path = new ArrayList<>();
        Set<String> visited = new HashSet<>();
        parser.traverseGraph(initialState, path, diagnosticPaths, visited, null);
    }

}