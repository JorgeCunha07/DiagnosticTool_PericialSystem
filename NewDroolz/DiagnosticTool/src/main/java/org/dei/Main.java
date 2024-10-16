package org.dei;

import org.dei.facts.model.Carro;
import org.dei.service.DiagnosticService;
import org.dei.service.CarSelectionService;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Main {

    private static List<Carro> carros = new ArrayList<>();

    public static void main(String[] args) {
        try {
            // Carregar base de dados
            carros = ImportFile.carregarBaseDados("baseDados.csv");
            if (carros.isEmpty()) {
                throw new RuntimeException("A lista de carros está vazia ou não foi carregada corretamente.");
            }

            Scanner scanner = new Scanner(System.in);
            String continuar;

            do {
                // Seleção do carro
                CarSelectionService carSelectionService = new CarSelectionService();
                Carro selectedCar = carSelectionService.selecionarCarro(carros);

                if (selectedCar != null) {
                    System.out.println("Carro selecionado:");
                    System.out.println("Marca: " + selectedCar.getMarca().getNome());
                    System.out.println("Modelo: " + selectedCar.getModelo().getNome());
                    System.out.println("Motor: " + selectedCar.getMotor().getNome());

                    // Iniciar diagnóstico
                    DiagnosticService diagnosticService = new DiagnosticService();
                    diagnosticService.iniciarDiagnostico(selectedCar);
                } else {
                    System.out.println("Nenhum carro foi selecionado.");
                }

                // Perguntar ao usuário se ele quer continuar
                System.out.print("Deseja realizar outro diagnóstico? (Sim/Não): ");
                continuar = scanner.nextLine().trim();

            } while (continuar.equalsIgnoreCase("Sim"));

            System.out.println("Programa encerrado.");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
