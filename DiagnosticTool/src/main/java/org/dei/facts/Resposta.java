package org.dei.facts;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.dei.facts.model.Carro;

import java.util.ArrayList;
import java.util.List;

/**
 * The Resposta class represents a response in the diagnostic tool, containing various details
 * about the selected car, diagnostic results, and related evidences.
 */
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Resposta {
    private String texto; // The text of the response
    private String estado; // The state of the response (e.g., "finalizado")
    private String pergunta; // The question related to the response

    private Carro carroSelecionado; // The selected car
    private String marcaSelecionada; // The selected car brand
    private String modeloSelecionado; // The selected car model
    private String motorSelecionado; // The selected car engine

    private String diagnostico; // The diagnostic result
    private String solucao; // The solution for the diagnostic
    private String explicacaoGeral; // General explanation
    private String explicacaoGeralNao; // General explanation for a negative result
    private String como; // Explanation of how the diagnostic was reached
    private List<Evidence<?,?>> evidencias; // List of evidences
    private List<String> triggeredRules; // List of triggered rules

    /**
     * Adds an evidence to the list of evidences.
     *
     * @param evidencia the evidence to add
     */
    public void addEvidencia(Evidence<?, ?> evidencia) {
        if (this.evidencias == null) {
            this.evidencias = new ArrayList<>();
        }
        this.evidencias.add(evidencia);
    }

    /**
     * Adds a triggered rule to the list of triggered rules.
     *
     * @param regra the rule to add
     */
    public void addRegraDisparada(String regra) {
        if (this.triggeredRules == null) {
            this.triggeredRules = new ArrayList<>();
        }
        this.triggeredRules.add(regra);
    }

    /**
     * Checks if the diagnostic is concluded.
     *
     * @return true if the diagnostic is concluded, false otherwise
     */
    public boolean isDiagnosticoConcluido() {
        return "finalizado".equalsIgnoreCase(this.estado);
    }
}