package org.dei.controller;

import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.dei.service.DiagnosticService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

@RestController
@RequestMapping("/api/diagnostico")
public class DiagnosticController {

    private final DiagnosticService diagnosticService;
    private final Map<String, Resposta> sessionStore = new HashMap<>(); // Armazena o estado da sessão de diagnóstico

    public DiagnosticController(DiagnosticService diagnosticService) {
        this.diagnosticService = diagnosticService;
    }

    // Iniciar diagnóstico
    @PostMapping("/iniciar")
    public ResponseEntity<Resposta> iniciarDiagnostico(@RequestParam String sessionId, @RequestBody Carro selectedCar) {
        Resposta resposta = diagnosticService.iniciarDiagnostico(selectedCar);
        sessionStore.put(sessionId, resposta); // Salva o estado inicial da sessão
        return ResponseEntity.ok(resposta);
    }

    // Processar resposta de uma pergunta
    @PostMapping("/responder")
    public ResponseEntity<Resposta> processarResposta(@RequestParam String sessionId, @RequestParam String respostaTexto) {
        Resposta resposta = sessionStore.get(sessionId);
        if (resposta == null) {
            return ResponseEntity.badRequest().body(null); // Se não houver sessão, retorne erro
        }

        // Chama o serviço para processar a resposta do usuário
        Resposta novaResposta = diagnosticService.processarResposta(resposta, respostaTexto);
        sessionStore.put(sessionId, novaResposta); // Atualiza o estado da sessão

        return ResponseEntity.ok(novaResposta);
    }
}
