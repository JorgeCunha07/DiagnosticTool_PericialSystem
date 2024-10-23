package org.dei.controller;

import org.dei.facts.Resposta;
import org.dei.facts.Why;
import org.dei.facts.model.Carro;
import org.dei.service.DiagnosticService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

@RestController
@RequestMapping("/api/diagnostico")
public class DiagnosticController {

    private final DiagnosticService diagnosticService;

    public DiagnosticController(DiagnosticService diagnosticService) {
        this.diagnosticService = diagnosticService;
    }

    @PostMapping("/iniciar")
    public ResponseEntity<Resposta> iniciarDiagnostico(@RequestBody Carro selectedCar) {
        Resposta resposta = diagnosticService.iniciarDiagnostico(selectedCar);
        return ResponseEntity.ok(resposta);
    }

    @PostMapping("/responder")
    public ResponseEntity<Resposta> processarResposta(@RequestBody Resposta resposta) {
        Resposta novaResposta = diagnosticService.processarResposta(resposta);
        return ResponseEntity.ok(novaResposta);
    }

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

}
