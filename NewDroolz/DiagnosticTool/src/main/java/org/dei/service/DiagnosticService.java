package org.dei.service;

import lombok.extern.slf4j.Slf4j;
import org.dei.facts.How;
import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.dei.whynot.DroolsWithWhyNot;
import org.kie.api.runtime.KieSession;
import org.kie.api.runtime.rule.FactHandle;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class DiagnosticService {

    private KieSession diagSession;
    private FactHandle respostaHandle;
    private DroolsWithWhyNot drools;

    // Inicializa o diagnóstico e retorna a primeira pergunta
    public Resposta iniciarDiagnostico(Carro selectedCar) {
        try {
            if (selectedCar == null) {
                log.error("Selected car is null. Cannot start diagnostic.");
                return null;
            }

            log.info("Starting diagnostic for car: " + selectedCar.getMarca() + " " + selectedCar.getModelo());

            // Verifica se a sessão Drools com WhyNot já foi inicializada
            if (!DroolsWithWhyNot.isInitialized()) {
                log.info("Initializing DroolsWithWhyNot...");
                this.drools = DroolsWithWhyNot.init("org.dei.facts");
            } else {
                log.info("DroolsWithWhyNot already initialized. Retrieving existing instance...");
                this.drools = DroolsWithWhyNot.getInstance();
            }

            // Reinitialize KieSession if it is null
            if (this.drools.getKieSession() == null) {
                log.info("KieSession is null. Reinitializing...");
                this.diagSession = drools.getKnowledgeBase().getKieBase().newKieSession();
            } else {
                log.info("KieSession is already initialized. Reusing existing session...");
                this.diagSession = drools.getKieSession();
            }

            if (this.diagSession == null) {
                log.error("Failed to initialize KieSession. It is still null.");
                return null;
            }

            log.info("KieSession successfully initialized. Inserting global selectedCar into session.");
            diagSession.setGlobal("selectedCar", selectedCar);

            // Create new Resposta object for diagnostic session
            Resposta diagResposta = new Resposta();
            diagResposta.setEstado("iniciarDiagnostico");
            diagResposta.setTexto("");
            diagResposta.setCarroSelecionado(selectedCar);

            this.respostaHandle = diagSession.insert(diagResposta);

            // Fire rules and return first question
            log.info("Firing rules to start the diagnostic session...");
            diagSession.fireAllRules();
            log.info("Rules fired. Diagnostic started.");

            return diagResposta;
        } catch (Exception e) {
            log.error("Exception occurred while initiating diagnosis: ", e);
            return null;
        }
    }

    // Processa as respostas subsequentes e retorna a próxima pergunta
    public Resposta processarResposta(Resposta diagResposta) {
        try {
            log.info("Processing response for the diagnostic...");

            if (!diagResposta.isDiagnosticoConcluido()) {
                log.info("Updating KieSession with the latest diagnostic response.");
                diagSession.update(respostaHandle, diagResposta);
                log.info("Firing rules for the next step in the diagnostic.");
                diagSession.fireAllRules();

                if (diagResposta.isDiagnosticoConcluido()) {
                    log.info("Diagnostic concluded. Generating explanation (How).");
                    How how = new How(diagResposta.getEvidencias());
                    String value = how.gerarExplicacao();
                    diagResposta.setComo(value);
                    log.info("Explanation generated: " + value);

                    log.info("Disposing of Drools and KieSession.");
                    drools.dispose();
                    diagSession.dispose();
                } else {
                    log.info("Diagnostic not yet concluded. Awaiting further responses.");
                }
            }

            return diagResposta;
        } catch (Exception e) {
            log.error("Exception occurred while processing the response: ", e);
            return null;
        }
    }
}
