package org.dei.facts;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.dei.facts.model.Carro;

import java.util.ArrayList;
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
    private String explicacaoGeral;
    private String explicacaoGeralNao;
    private String como;
    private List<Evidence<String,String>> evidencias;
    private List<String> triggeredRules;

    public void addEvidencia(Evidence<String, String> evidencia) {
        if (this.evidencias == null) {
            this.evidencias = new ArrayList<>();
        }
        this.evidencias.add(evidencia);
    }

    public void addRegraDisparada(String regra) {
        this.triggeredRules.add(regra);
    }

    public boolean isDiagnosticoConcluido() {
        return "finalizado".equalsIgnoreCase(this.estado);
    }
}
