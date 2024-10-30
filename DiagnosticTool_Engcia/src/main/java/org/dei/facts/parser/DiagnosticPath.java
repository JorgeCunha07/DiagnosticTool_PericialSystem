package org.dei.facts.parser;

import lombok.Getter;

import java.util.*;

/**
 * The DiagnosticPath class represents a diagnostic path with a diagnosis, a list of state names, and a list of applied rules.
 */
@Getter
public class DiagnosticPath {
    public String diagnosis; // The diagnosis associated with the path
    public List<String> path; // The list of state names in the path
    public List<String> rules; // The list of applied rule names

    /**
     * Constructs a DiagnosticPath with the specified diagnosis, path, and rules.
     *
     * @param diagnosis the diagnosis associated with the path
     * @param path the list of state names in the path
     * @param rules the list of applied rule names
     */
    public DiagnosticPath(String diagnosis, List<String> path, List<String> rules) {
        this.diagnosis = (diagnosis != null) ? diagnosis : "Diagnóstico não especificado"; // Set diagnosis or default if null
        this.path = path;
        this.rules = rules;
    }
}