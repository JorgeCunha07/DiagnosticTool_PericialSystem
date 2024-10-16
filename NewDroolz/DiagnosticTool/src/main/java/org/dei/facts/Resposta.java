package org.dei.facts;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.dei.facts.model.Carro;

import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Resposta {
    private String texto;
    private String estado;
    private String pergunta;

    private Carro carroSelecionado;
    private String marcaSelecionada;
    private String modeloSelecionado;
    private String motorSelecionado;

    private String diagnostico;
    private String solucao;
    private String porque;
    private String porqueNao;
    private List<Evidence<?,?>> evidencias;


}
