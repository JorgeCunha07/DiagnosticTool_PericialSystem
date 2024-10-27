package org.dei.facts.parser;

import lombok.Getter;

import java.util.*;

public class StateNode {
    String estadoName;
    @Getter
    List<Transition> transitions = new ArrayList<>();
    private boolean isDiagnosisState;
    @Getter
    private String diagnosis;

    public StateNode(String estadoName) {
        this.estadoName = estadoName;
    }

    public void addTransition(Transition transition) {
        transitions.add(transition);
    }

    public boolean isDiagnosisState() {
        return isDiagnosisState;
    }

    public void setDiagnosis(String diagnosis) {
        this.diagnosis = diagnosis;
        this.isDiagnosisState = true;
    }

}
