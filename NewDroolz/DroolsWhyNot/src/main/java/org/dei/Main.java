package org.dei;

import org.kie.api.KieServices;
import org.kie.api.builder.KieBuilder;
import org.kie.api.builder.KieFileSystem;
import org.kie.api.builder.Message;
import org.kie.api.builder.Results;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.dei.facts.Resposta;
import org.dei.facts.model.*;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;

public class Main {

    private static List<Carro> carros = new ArrayList<>();

    public static void main(String[] args) {
        try {
            // Initialize KieServices and build the rules
            KieServices ks = KieServices.Factory.get();
            KieContainer kContainer = ks.getKieClasspathContainer();
            KieFileSystem kfs = ks.newKieFileSystem();
            KieBuilder kieBuilder = ks.newKieBuilder(kfs);
            kieBuilder.buildAll();
            org.slf4j.Logger LOG = LoggerFactory.getLogger(Main.class);

            Results results = kieBuilder.getResults();
            if (results.hasMessages(Message.Level.ERROR)) {
                for (Message message : results.getMessages(Message.Level.ERROR)) {
                    System.out.println("Erro: " + message.getText());
                }
                throw new IllegalStateException("Compilação do DRL falhou.");
            }

            // Load KieSession
            KieSession kSession = kContainer.newKieSession("ksession-rules");

            // Check if the session loaded correctly
            if (kSession == null) {
                throw new RuntimeException("Failed to load KieSession.");
            }

            // Load car data
            carros = carregarBaseDados("baseDados.csv");
            if (carros.isEmpty()) {
                throw new RuntimeException("No car data loaded.");
            }
            //System.out.println("Car data loaded: " + carros);

            // Set global variables
            kSession.setGlobal("carros", carros);
            List<String> triggeredRules = new ArrayList<>();
            kSession.setGlobal("triggeredRules", triggeredRules);
            kSession.setGlobal("LOG", LOG);
            // Prepare for user interaction
            Scanner scanner = new Scanner(System.in);
            Resposta resposta = new Resposta();

            // Ask the user for inputs and trigger rules step by step
            System.out.println("Qual é a marca do carro?");
            resposta.setTexto(scanner.nextLine());
            kSession.insert(resposta);
            System.out.println("Fato inserido: " + resposta.getTexto());
            System.out.println("Disparando todas as regras...");
            kSession.fireAllRules();
            System.out.println("Regras disparadas.");

            System.out.println("Qual é o modelo do carro?");
            resposta.setTexto(scanner.nextLine());
            kSession.insert(resposta);
            System.out.println("Fato inserido: " + resposta.getTexto());
            System.out.println("Disparando todas as regras...");
            kSession.fireAllRules();
            System.out.println("Regras disparadas.");


            System.out.println("Qual é o motor do carro?");
            resposta.setTexto(scanner.nextLine());
            kSession.insert(resposta);
            System.out.println("Fato inserido: " + resposta.getTexto());
            System.out.println("Disparando todas as regras...");
            kSession.fireAllRules();
            System.out.println("Regras disparadas.");


            // Finalize session
            kSession.dispose();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Capture user response method
    private static void capturarResposta(Scanner scanner, String pergunta, Resposta resposta) {
        System.out.println(pergunta);
        String entrada = scanner.nextLine();
        resposta.setTexto(entrada);
    }

    // Load data from CSV
    private static List<Carro> carregarBaseDados(String caminhoArquivo) {
        List<Carro> carros = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(caminhoArquivo))) {
            br.readLine(); // Ignore header
            String linha;
            while ((linha = br.readLine()) != null) {
                processarLinha(linha, carros);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return carros;
    }

    // Process each line from CSV
    private static void processarLinha(String linha, List<Carro> carros) {
        String[] valores = linha.split(",");
        String nomeMarca = valores[0].trim();
        String nomeModelo = valores[1].trim();
        String nomeMotor = valores[2].trim();
        String nomeComponente = valores[3].trim();
        double valorMinimo = Double.parseDouble(valores[4].trim());
        double valorMaximo = Double.parseDouble(valores[5].trim());

        Marca marca = obterOuCriarMarca(nomeMarca, carros);
        ModeloCarro modeloCarro = obterOuCriarModelo(marca, nomeModelo);
        Motor motor = obterOuCriarMotor(modeloCarro, nomeMotor);
        Carro carro = obterOuCriarCarro(marca, modeloCarro, motor, carros);

        Componente componente = new Componente(nomeComponente, valorMinimo, valorMaximo);
        carro.adicionarComponente(componente);
    }

    private static Marca obterOuCriarMarca(String nomeMarca, List<Carro> carros) {
        return carros.stream()
                .map(Carro::getMarca)
                .filter(marca -> marca.getNome().equals(nomeMarca))
                .findFirst()
                .orElseGet(() -> {
                    Marca novaMarca = new Marca(nomeMarca, new ArrayList<>());
                    return novaMarca;
                });
    }

    private static ModeloCarro obterOuCriarModelo(Marca marca, String nomeModelo) {
        return marca.getModelos().stream()
                .filter(m -> m.getNome().equals(nomeModelo))
                .findFirst()
                .orElseGet(() -> {
                    ModeloCarro novoModelo = new ModeloCarro(nomeModelo, new ArrayList<>());
                    marca.getModelos().add(novoModelo);
                    return novoModelo;
                });
    }

    private static Motor obterOuCriarMotor(ModeloCarro modeloCarro, String nomeMotor) {
        return modeloCarro.getMotores().stream()
                .filter(m -> m.getNome().equals(nomeMotor))
                .findFirst()
                .orElseGet(() -> {
                    Motor novoMotor = new Motor(nomeMotor);
                    modeloCarro.getMotores().add(novoMotor);
                    return novoMotor;
                });
    }

    private static Carro obterOuCriarCarro(Marca marca, ModeloCarro modeloCarro, Motor motor, List<Carro> carros) {
        if (marca == null || modeloCarro == null || motor == null) {
            throw new RuntimeException("Erro: marca, modeloCarro ou motor é null");
        }

        return carros.stream()
                .filter(c -> c.getMarca().equals(marca) && c.getModelo().equals(modeloCarro) && c.getMotor().equals(motor))
                .findFirst()
                .orElseGet(() -> {
                    Carro novoCarro = new Carro(marca, modeloCarro, motor);
                    carros.add(novoCarro);
                    return novoCarro;
                });
    }
}
