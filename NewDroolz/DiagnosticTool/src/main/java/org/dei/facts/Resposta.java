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
    private String texto;  // Texto da resposta fornecida pelo usuário
    private String estado; // Estado do processo atual (ex.: "iniciarDiagnostico", "finalizado")
    private String pergunta; // Pergunta atual a ser exibida ao usuário

    // Informações sobre o carro selecionado
    private Carro carroSelecionado;
    private String marcaSelecionada;
    private String modeloSelecionado;
    private String motorSelecionado;

    // Diagnóstico gerado pelo sistema Drools
    private String diagnostico;
    private String solucao;
    private String porque;
    private String porqueNao;

    // Lista de evidências coletadas ao longo do diagnóstico
    private List<Evidence<String, String>> evidencias;

    // Explicações do WhyNot sobre por que algumas regras não foram disparadas
    private List<String> explicacoesPorqueNao = new ArrayList<>();

    // Lista de perguntas adicionais geradas pelo WhyNot
    private List<String> perguntasWhyNot = new ArrayList<>();

    // Lista de regras que foram disparadas
    private List<String> regrasDisparadas = new ArrayList<>();

    // Métodos auxiliares para adicionar itens às listas

    // Adiciona uma explicação do WhyNot sobre por que uma conclusão não foi atingida
    public void addExplicacaoPorqueNao(String explicacao) {
        this.explicacoesPorqueNao.add(explicacao);
    }

    // Adiciona uma evidência ao conjunto de evidências
    public void addEvidencia(Evidence<String, String> evidencia) {
        if (this.evidencias == null) {
            this.evidencias = new ArrayList<>();
        }
        this.evidencias.add(evidencia);
    }

    public void addPerguntaWhyNot(String pergunta) {
        if (this.perguntasWhyNot == null) {
            this.perguntasWhyNot = new ArrayList<>();
        }
        this.perguntasWhyNot.add(pergunta);
    }

    public void addRegraDisparada(String regra) {
        if (this.regrasDisparadas == null) {
            this.regrasDisparadas = new ArrayList<>();
        }
        this.regrasDisparadas.add(regra);
    }

    // Métodos para verificar se o diagnóstico foi concluído
    public boolean isDiagnosticoConcluido() {
        return "finalizado".equalsIgnoreCase(this.estado);
    }
}
