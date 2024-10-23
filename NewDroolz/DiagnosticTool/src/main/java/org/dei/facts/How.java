package org.dei.facts;

import java.util.List;
import java.util.StringJoiner;

public class How {

    private final List<Evidence<?, ?>> evidencias;

    public How(List<Evidence<?,?>> evidencias) {
        this.evidencias = evidencias;
    }

    // Gerar a explicação de como o diagnóstico foi concluído
    public String gerarExplicacao() {
        StringJoiner explicacao = new StringJoiner("\n", "Processo de inferência baseado nas seguintes evidências:\n", "");
        for (Evidence<?, ?> evidencia : evidencias) {
            explicacao.add("Rule: " + evidencia.getFact() + " , Value: " + evidencia.getValue());
        }
        return explicacao.toString();
    }
}