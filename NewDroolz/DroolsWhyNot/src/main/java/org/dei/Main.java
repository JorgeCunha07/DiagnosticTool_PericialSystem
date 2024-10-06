package org.dei;

import org.kie.api.KieServices;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Scanner;

public class Main {

    private static List<Carro> carros = new ArrayList<>();

    public static void main(String[] args) {
        try {
            // Initialize KieServices and KieContainer
            KieServices kieServices = KieServices.Factory.get();
            KieContainer kieContainer = kieServices.getKieClasspathContainer();
            KieSession kSession = kieContainer.newKieSession("ksession-rules");

            // Load data and insert facts
            carros = ImportFile.carregarBaseDados("baseDados.csv");
            System.out.println("Carros loaded: ");
            for (Carro carro : carros) {
                System.out.println(carro);  // Print each car to ensure they're loaded
                kSession.insert(carro);  // Insert Car objects into session
            }

            // Set global variables
            kSession.setGlobal("carros", carros);
            kSession.setGlobal("triggeredRules", new ArrayList<String>());
            kSession.setGlobal("LOG", LoggerFactory.getLogger(Main.class));

            // Verify facts in session
            System.out.println("Number of facts in session: " + kSession.getFactCount());

            // Print all facts currently in the session
            Collection<?> facts = kSession.getObjects();
            for (Object fact : facts) {
                System.out.println(fact);
            }

            // Capture user responses and fire rules
            Scanner scanner = new Scanner(System.in);
            Resposta resposta = new Resposta();

            // Perguntar Marca
            capturarResposta(scanner, "Qual é a marca do carro?", resposta);
            kSession.insert(resposta);
            int fired = kSession.fireAllRules();
            System.out.println("Number of rules fired: " + fired);

            // Perguntar Modelo
            resposta = new Resposta();  // Reset resposta
            capturarResposta(scanner, "Qual é o modelo do carro?", resposta);
            kSession.insert(resposta);
            fired = kSession.fireAllRules();
            System.out.println("Number of rules fired: " + fired);

            // Perguntar Motor
            resposta = new Resposta();  // Reset resposta
            capturarResposta(scanner, "Qual é o motor do carro?", resposta);
            kSession.insert(resposta);
            fired = kSession.fireAllRules();
            System.out.println("Number of rules fired: " + fired);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void capturarResposta(Scanner scanner, String pergunta, Resposta resposta) {
        System.out.println(pergunta);
        String input = scanner.nextLine();
        resposta.setTexto(input);
        System.out.println("Resposta inserida: " + resposta.getTexto());
    }
}