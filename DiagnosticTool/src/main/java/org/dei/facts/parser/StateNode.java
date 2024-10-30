package org.dei.facts.parser;

import lombok.Getter;

import java.util.*;

/**
 * The StateNode class represents a state in the diagnostic state graph.
 * It contains the state name, transitions to other states, and diagnostic information.
 */
public class StateNode {
    @Getter
    private String estadoName; // The name of the state
    @Getter
    private List<Transition> transitions = new ArrayList<>(); // The list of transitions from this state
    private boolean isDiagnosisState; // Indicates if this state is a diagnosis state
    @Getter
    private String diagnosis; // The diagnosis associated with this state

    /**
     * Constructs a StateNode with the specified state name.
     *
     * @param estadoName the name of the state
     */
    public StateNode(String estadoName) {
        this.estadoName = estadoName;
    }

    /**
     * Adds a transition to the list of transitions from this state.
     *
     * @param transition the Transition object to add
     */
    public void addTransition(Transition transition) {
        transitions.add(transition);
    }

    /**
     * Checks if this state is a diagnosis state.
     *
     * @return true if this state is a diagnosis state, false otherwise
     */
    public boolean isDiagnosisState() {
        return isDiagnosisState;
    }

    /**
     * Sets the diagnosis for this state and marks it as a diagnosis state.
     *
     * @param diagnosis the diagnosis to set
     */
    public void setDiagnosis(String diagnosis) {
        this.diagnosis = diagnosis;
        this.isDiagnosisState = true;
    }
}