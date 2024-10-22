package org.dei.facts;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Why {
    private List<Evidence<?,?>> evidencias;

    // Obter a pergunta anterior que guiou para a pergunta sugerida
    public String obterPerguntaAnterior(String perguntaAtual) {
        for (int i = 1; i < evidencias.size(); i++) {
            if (evidencias.get(i).toString().contains(perguntaAtual)) {
                Evidence<?, ?> evidenciaAnterior = evidencias.get(i - 1);
                return "Pergunta anterior: " + evidenciaAnterior.toString();
            }
        }
        return "Pergunta não encontrada na lista de evidências.";
    }
}
