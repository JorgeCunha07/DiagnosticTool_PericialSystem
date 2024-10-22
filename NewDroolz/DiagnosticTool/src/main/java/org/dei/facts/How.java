package org.dei.facts;

import java.util.List;

public class How {

    // Gerar a explicação de como o diagnóstico foi concluído
    public static String gerarExplicacao(List<Evidence<?, ?>> evidencias) {
        StringBuilder explicacao = new StringBuilder("Processo de inferência baseado nas seguintes evidências:\n");
        for (Evidence<?, ?> evidencia : evidencias) {
            explicacao.append(evidencia.toString()).append("\n");
        }
        return explicacao.toString();
    }
}