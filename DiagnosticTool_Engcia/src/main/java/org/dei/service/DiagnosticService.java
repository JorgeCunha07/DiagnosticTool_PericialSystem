package org.dei.service;

import lombok.extern.slf4j.Slf4j;
import org.dei.facts.How;
import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.dei.facts.parser.DiagnosticPath;
import org.dei.whynot.DroolsWithWhyNot;
import org.kie.api.runtime.KieSession;
import org.kie.api.runtime.rule.FactHandle;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Service class for handling diagnostic operations.
 */
@Slf4j
@Service
public class DiagnosticService {

    private KieSession diagSession;
    private FactHandle respostaHandle;
    private DroolsWithWhyNot drools;
    private final DiagnosticParserService diagnosticParserService;

    /**
     * Constructor for DiagnosticService.
     *
     * @param diagnosticParserService the service for parsing diagnostic files
     */
    public DiagnosticService(DiagnosticParserService diagnosticParserService) {
        this.diagnosticParserService = diagnosticParserService;
    }

    /**
     * Obtains the diagnostic paths by processing the diagnostic file.
     *
     * @return a list of DiagnosticPath objects
     */
    public List<DiagnosticPath> obterDiagnosticPaths() {
        String filePath = "src/main/resources/org/dei/diagnostic.drl";
        diagnosticParserService.processDiagnosticFile(filePath);
        return diagnosticParserService.getDiagnosticPaths();
    }

    /**
     * Initiates the diagnostic process for the selected car.
     *
     * @param selectedCar the car selected for diagnosis
     * @return a Resposta object containing the diagnostic response
     */
    public Resposta iniciarDiagnostico(Carro selectedCar) {
        try {
            if (selectedCar == null) {
                log.error("Selected car is null. Cannot start diagnostic.");
                return null;
            }

            log.info("Starting diagnostic for car: " + selectedCar.getMarca() + " " + selectedCar.getModelo());

            if (!DroolsWithWhyNot.isInitialized()) {
                log.info("Initializing DroolsWithWhyNot...");
                this.drools = DroolsWithWhyNot.init("org.dei.facts");
            } else {
                log.info("DroolsWithWhyNot already initialized. Retrieving existing instance...");
                this.drools = DroolsWithWhyNot.getInstance();
            }

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

            Resposta diagResposta = new Resposta();
            diagResposta.setEstado("iniciarDiagnostico");
            diagResposta.setTexto("");
            diagResposta.setCarroSelecionado(selectedCar);

            this.respostaHandle = diagSession.insert(diagResposta);

            log.info("Firing rules to start the diagnostic session...");
            diagSession.fireAllRules();
            log.info("Rules fired. Diagnostic started.");

            return diagResposta;
        } catch (Exception e) {
            log.error("Exception occurred while initiating diagnosis: ", e);
            return null;
        }
    }

    /**
     * Processes the response for the diagnostic.
     *
     * @param diagResposta the diagnostic response to process
     * @return the updated Resposta object
     */
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

    /**
     * Retrieves the missing rules for alternative diagnoses.
     *
     * @param resposta the diagnostic response
     * @return a map of missing rules by diagnosis
     */
    public Map<String, List<String>> getMissingRulesForAlternativeDiagnoses(Resposta resposta) {
        if (diagnosticParserService.getDiagnosticPaths().isEmpty()) {
            obterDiagnosticPaths();
        }
        List<DiagnosticPath> diagnosticPaths = diagnosticParserService.getDiagnosticPaths();

        List<String> triggeredRules = resposta.getTriggeredRules();
        Map<String, List<String>> missingRulesByDiagnosis = new HashMap<>();

        for (DiagnosticPath dp : diagnosticPaths) {
            List<String> missingRules = dp.getRules().stream()
                    .filter(rule -> !triggeredRules.contains(rule))
                    .collect(Collectors.toList());

            if (!missingRules.isEmpty()) {
                missingRulesByDiagnosis.put(dp.getDiagnosis(), missingRules);
            }
        }
        return missingRulesByDiagnosis;
    }

}