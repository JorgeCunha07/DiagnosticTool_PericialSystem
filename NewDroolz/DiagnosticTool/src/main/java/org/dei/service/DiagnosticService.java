package org.dei.service;

import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.dei.whynot.DroolsWithWhyNot;
import org.kie.api.runtime.KieSession;
import org.kie.api.runtime.rule.FactHandle;
import org.springframework.stereotype.Service;

@Service
public class DiagnosticService {

    private KieSession diagSession;
    private FactHandle respostaHandle;

    // Inicializa o diagnóstico e retorna a primeira pergunta
    public Resposta iniciarDiagnostico(Carro selectedCar) {
        try {
            // Inicializa a sessão Drools com WhyNot
            DroolsWithWhyNot drools = DroolsWithWhyNot.init("org.dei.facts");
            this.diagSession = drools.getKieSession();

            // Define o carro selecionado como variável global
            diagSession.setGlobal("selectedCar", selectedCar);

            // Cria um novo objeto Resposta para a sessão de diagnóstico
            Resposta diagResposta = new Resposta();
            diagResposta.setEstado("iniciarDiagnostico");
            diagResposta.setTexto("");
            diagResposta.setCarroSelecionado(selectedCar);

            this.respostaHandle = diagSession.insert(diagResposta);

            // Processa as primeiras regras e retorna a primeira pergunta
            diagSession.fireAllRules();
            return diagResposta;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    // Processa as respostas subsequentes e retorna a próxima pergunta
    public Resposta processarResposta(Resposta diagResposta, String respostaTexto) {
        try {
            diagResposta.setTexto(respostaTexto);
            diagSession.update(respostaHandle, diagResposta);
            diagSession.fireAllRules();
            return diagResposta;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}
