package org.dei.controller;

import org.dei.ImportFile;
import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.dei.service.CarSelectionService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api")
public class CarSelectionController {

    private final CarSelectionService carSelectionService;
    private final List<Carro> carros;

    public CarSelectionController(CarSelectionService carSelectionService) {
        this.carSelectionService = carSelectionService;
        this.carros = ImportFile.carregarBaseDados("src/main/resources/baseDados.csv");
    }

    @GetMapping("/carro/start")
    public ResponseEntity<Resposta> iniciarSelecao() {
        Resposta resposta = carSelectionService.processarResposta(carros, "");
        return ResponseEntity.ok(resposta);
    }

    @PostMapping("/carro/selection")
    public ResponseEntity<Resposta> processarResposta(@RequestParam String value) {
        Resposta resposta = carSelectionService.processarResposta(carros, value);
        return ResponseEntity.ok(resposta);
    }

    @GetMapping("/carros")
    public ResponseEntity<List<Carro>> buscarListaCarros() {
        return ResponseEntity.ok(carros);
    }
}
