package org.dei.controller;

import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.dei.service.DiagnosticService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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
}
