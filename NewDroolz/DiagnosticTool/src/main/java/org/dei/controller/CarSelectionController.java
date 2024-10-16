package org.dei.controller;

import org.dei.ImportFile;
import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.dei.service.CarSelectionService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/carro")
public class CarSelectionController {

    private final CarSelectionService carSelectionService;
    private final List<Carro> carros;

    public CarSelectionController(CarSelectionService carSelectionService) {
        this.carSelectionService = carSelectionService;
        this.carros = ImportFile.carregarBaseDados("baseDados.csv");
    }

    @GetMapping("/start")
    public ResponseEntity<Resposta> iniciarSelecao() {
        Resposta resposta = carSelectionService.processarResposta(carros, "");
        return ResponseEntity.ok(resposta);
    }

    @PostMapping("/selection")
    public ResponseEntity<Resposta> selecionarMarca(@RequestParam String value) {
        Resposta resposta = carSelectionService.processarResposta(carros, value);
        return ResponseEntity.ok(resposta);
    }
}
