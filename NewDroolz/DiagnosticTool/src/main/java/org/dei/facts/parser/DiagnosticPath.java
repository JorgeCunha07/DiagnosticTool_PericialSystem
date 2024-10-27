package org.dei.facts.parser;

import java.util.*;

public class DiagnosticPath {
    public String diagnosis;
    public List<String> path;

    public DiagnosticPath(String diagnosis, List<String> path) {
        this.diagnosis = (diagnosis != null) ? diagnosis : "Diagnóstico não especificado";
        this.path = path;
    }
}
