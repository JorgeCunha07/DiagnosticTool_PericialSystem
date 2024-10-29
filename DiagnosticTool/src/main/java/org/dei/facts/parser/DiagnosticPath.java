package org.dei.facts.parser;

import lombok.Getter;

import java.util.*;

@Getter
public class DiagnosticPath {
    public String diagnosis;
    public List<String> path;
    public List<String> rules; // Novo campo para armazenar o nome das regras aplicadas

    public DiagnosticPath(String diagnosis, List<String> path, List<String> rules) {
        this.diagnosis = (diagnosis != null) ? diagnosis : "Diagnóstico não especificado";
        this.path = path;
        this.rules = rules;
    }
}
