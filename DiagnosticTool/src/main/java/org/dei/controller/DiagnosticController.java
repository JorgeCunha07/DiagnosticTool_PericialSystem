package org.dei.controller;

import org.dei.facts.Resposta;
import org.dei.facts.Why;
import org.dei.facts.model.Carro;
import org.dei.facts.parser.DiagnosticPath;
import org.dei.service.DiagnosticService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * REST controller for handling diagnostic-related requests.
 */
@RestController
@RequestMapping("/api/diagnostico")
public class DiagnosticController {

    private final DiagnosticService diagnosticService;

    /**
     * Constructor for DiagnosticController.
     *
     * @param diagnosticService the service to handle diagnostic logic
     */
    public DiagnosticController(DiagnosticService diagnosticService) {
        this.diagnosticService = diagnosticService;
    }

    /**
     * Starts the diagnostic process for a selected car.
     *
     * @param selectedCar the car selected for diagnosis
     * @return a ResponseEntity containing the initial Resposta object
     */
    @PostMapping("/iniciar")
    public ResponseEntity<Resposta> iniciarDiagnostico(@RequestBody Carro selectedCar) {
        Resposta resposta = diagnosticService.iniciarDiagnostico(selectedCar);
        return ResponseEntity.ok(resposta);
    }

    /**
     * Processes the user's response during the diagnostic process.
     *
     * @param resposta the user's response
     * @return a ResponseEntity containing the updated Resposta object
     */
    @PostMapping("/responder")
    public ResponseEntity<Resposta> processarResposta(@RequestBody Resposta resposta) {
        Resposta novaResposta = diagnosticService.processarResposta(resposta);
        return ResponseEntity.ok(novaResposta);
    }

    /**
     * Retrieves the previous question based on the current question and the given response.
     *
     * @param resposta the user's response
     * @param perguntaAtual the current question
     * @return a ResponseEntity containing the previous question or an error message
     */
    @PostMapping("/perguntaAnterior")
    public ResponseEntity<String> obterPerguntaAnterior(@RequestBody Resposta resposta, @RequestParam String perguntaAtual) {
        Optional<Why> whyOptional = resposta.getEvidencias() != null && !resposta.getEvidencias().isEmpty()
                ? Optional.of(new Why(resposta.getEvidencias()))
                : Optional.empty();

        if (whyOptional.isPresent()) {
            String perguntaAnterior = whyOptional.get().obterPerguntaAnterior(perguntaAtual);
            return ResponseEntity.ok(perguntaAnterior);
        }
        return ResponseEntity.badRequest().body("Nenhuma evidência encontrada para determinar a pergunta anterior.");
    }

    /**
     * Retrieves the diagnostic paths.
     *
     * @return a ResponseEntity containing the list of DiagnosticPath objects
     */
    @GetMapping("/caminhosDiagnostico")
    public ResponseEntity<List<DiagnosticPath>> obterCaminhosDiagnostico() {
        List<DiagnosticPath> diagnosticPaths = diagnosticService.obterDiagnosticPaths();
        return ResponseEntity.ok(diagnosticPaths);
    }

    /**
     * Retrieves the missing rules for alternative diagnoses based on the given response.
     *
     * @param resposta the user's response
     * @return a ResponseEntity containing a map of missing rules by diagnosis
     */
    @PostMapping("/caminhosDiagnostico")
    public ResponseEntity<Map<String, List<String>>> obterRegrasFaltantesParaDiagnosticosAlternativos(@RequestBody Resposta resposta) {
        Map<String, List<String>> missingRules = diagnosticService.getMissingRulesForAlternativeDiagnoses(resposta);
        return ResponseEntity.ok(missingRules);
    }
}