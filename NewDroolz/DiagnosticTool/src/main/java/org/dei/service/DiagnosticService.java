package org.dei.service;

import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.dei.whynot.DroolsWithWhyNot;
import org.dei.whynot.WhyNot;
import org.kie.api.runtime.KieSession;
import org.kie.api.runtime.rule.FactHandle;

import java.util.Scanner;

public class DiagnosticService {

    public void iniciarDiagnostico(Carro selectedCar) {
        try {
            // Inicializa a sessão Drools com WhyNot
            DroolsWithWhyNot drools = DroolsWithWhyNot.init("org.dei.facts");
            KieSession diagSession = drools.getKieSession();

            // Define o carro selecionado como variável global
            diagSession.setGlobal("selectedCar", selectedCar);

            // Define a instância do WhyNot como variável global
            WhyNot whyNot = WhyNot.getInstance();
            diagSession.setGlobal("whyNot", whyNot);

            // Cria um novo objeto Resposta para a sessão de diagnóstico
            Resposta diagResposta = new Resposta();
            diagResposta.setEstado("iniciarDiagnostico");
            diagResposta.setTexto("");
            diagResposta.setCarroSelecionado(selectedCar);

            FactHandle respostaHandle = diagSession.insert(diagResposta);

            Scanner scanner = new Scanner(System.in);

            while (!"finalizado".equals(diagResposta.getEstado())) {
                diagSession.fireAllRules();

                // Verifica o estado e processa a resposta do utilizador
                if (diagResposta.getEstado().startsWith("perguntar") || diagResposta.getEstado().startsWith("processar")) {
                    if (diagResposta.getTexto() == null || diagResposta.getTexto().isEmpty()) {
                        System.out.print("Digite sua resposta: ");
                        String input = scanner.nextLine();
                        diagResposta.setTexto(input);
                        diagSession.update(respostaHandle, diagResposta);
                    }
                }
            }

            diagSession.dispose();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
