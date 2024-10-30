package org.dei.controller;

import org.dei.facts.parser.ImportFile;
import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.dei.service.CarSelectionService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * REST controller for handling car selection related requests.
 */
@RestController
@RequestMapping("/api")
public class CarSelectionController {

    private final CarSelectionService carSelectionService;
    private final List<Carro> carros;

    /**
     * Constructor for CarSelectionController.
     *
     * @param carSelectionService the service to handle car selection logic
     */
    public CarSelectionController(CarSelectionService carSelectionService) {
        this.carSelectionService = carSelectionService;
        this.carros = ImportFile.carregarBaseDados("src/main/resources/baseDados.csv");
    }

    /**
     * Starts the car selection process.
     *
     * @return a ResponseEntity containing the initial Resposta object
     */
    @GetMapping("/carro/start")
    public ResponseEntity<Resposta> iniciarSelecao() {
        Resposta resposta = carSelectionService.processarResposta(carros, "");
        return ResponseEntity.ok(resposta);
    }

    /**
     * Processes the user's response during car selection.
     *
     * @param value the user's response value
     * @return a ResponseEntity containing the updated Resposta object
     */
    @PostMapping("/carro/selection")
    public ResponseEntity<Resposta> processarResposta(@RequestParam String value) {
        Resposta resposta = carSelectionService.processarResposta(carros, value);
        return ResponseEntity.ok(resposta);
    }

    /**
     * Retrieves the list of available cars.
     *
     * @return a ResponseEntity containing the list of Carro objects
     */
    @GetMapping("/carros")
    public ResponseEntity<List<Carro>> buscarListaCarros() {
        return ResponseEntity.ok(carros);
    }
}