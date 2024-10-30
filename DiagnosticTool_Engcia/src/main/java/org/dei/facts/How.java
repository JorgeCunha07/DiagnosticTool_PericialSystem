package org.dei.facts;

import java.util.List;
import java.util.StringJoiner;

/**
 * The How class generates an explanation of how a diagnosis was concluded based on a list of evidences.
 */
public class How {

    private final List<Evidence<?, ?>> evidencias;

    /**
     * Constructor for the How class.
     *
     * @param evidencias the list of evidences used for generating the explanation
     */
    public How(List<Evidence<?,?>> evidencias) {
        this.evidencias = evidencias;
    }

    /**
     * Generates an explanation of how the diagnosis was concluded.
     *
     * @return a string containing the explanation
     */
    public String gerarExplicacao() {
        StringJoiner explicacao = new StringJoiner("\n", "Processo de inferência baseado nas seguintes evidências:\n", "");
        for (Evidence<?, ?> evidencia : evidencias) {
            explicacao.add("Rule: " + evidencia.getFact() + " , Value: " + evidencia.getValue());
        }
        return explicacao.toString();
    }
}