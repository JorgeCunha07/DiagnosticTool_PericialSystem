package org.dei.facts;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

/**
 * The Why class represents a collection of evidences and provides a method to obtain the previous question
 * that led to the current suggested question.
 */
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Why {
    private List<Evidence<?,?>> evidencias;

    /**
     * Obtains the previous question that guided to the current suggested question.
     *
     * @param perguntaAtual the current question
     * @return the previous question as a string, or a message indicating the question was not found
     */
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