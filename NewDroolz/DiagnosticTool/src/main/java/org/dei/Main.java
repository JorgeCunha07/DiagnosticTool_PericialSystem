package org.dei;

import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.kie.api.KieServices;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.kie.api.runtime.rule.FactHandle;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Main {

    private static List<Carro> carros = new ArrayList<>();

    public static void main(String[] args) {
        try {
            // Carregar base de dados
            carros = ImportFile.carregarBaseDados("baseDados.csv");
            if (carros == null || carros.isEmpty()) {
                throw new RuntimeException("A lista de carros está vazia ou não foi carregada corretamente.");
            }

            Resposta resposta = new Resposta();
            resposta.setEstado(""); // Inicializa o estado com uma string vazia
            resposta.setTexto("");  // Inicializa o texto com uma string vazia

            // Configurar KieServices e KieSession
            KieServices ks = KieServices.Factory.get();
            KieContainer kc = ks.getKieClasspathContainer();
            KieSession kSession = kc.newKieSession("ksession-rules");

            // Definir variáveis globais
            kSession.setGlobal("carros", carros);
            kSession.setGlobal("triggeredRules", new ArrayList<String>());
            kSession.setGlobal("LOG", LoggerFactory.getLogger(Main.class));

            // Inserir o objeto resposta na sessão
            FactHandle respostaHandle = kSession.insert(resposta);

            Scanner scanner = new Scanner(System.in);

            while (!"finalizado".equals(resposta.getEstado())) {
                kSession.fireAllRules();

                String estado = resposta.getEstado();
                String texto = resposta.getTexto();

                if (estado != null && estado.startsWith("aguardando") && (texto == null || texto.isEmpty())) {
                    System.out.print("Digite sua resposta: ");
                    String input = scanner.nextLine();
                    resposta.setTexto(input);

                    // Atualiza o objeto resposta na sessão
                    if (respostaHandle != null) {
                        kSession.update(respostaHandle, resposta);
                    } else {
                        respostaHandle = kSession.insert(resposta);
                    }
                }
            }

            // Após a interação, você pode acessar o carro selecionado
            Carro selectedCar = resposta.getCarroSelecionado();
            if (selectedCar != null) {
                // Armazena o carro selecionado em uma variável para uso posterior
                System.out.println("Carro selecionado:");
                System.out.println("Marca: " + selectedCar.getMarca().getNome());
                System.out.println("Modelo: " + selectedCar.getModelo().getNome());
                System.out.println("Motor: " + selectedCar.getMotor().getNome());

                // Begin diagnostic process
                iniciarDiagnostico(selectedCar);
            } else {
                System.out.println("Nenhum carro foi selecionado.");
            }

            kSession.dispose();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // New method to handle diagnostic
    public static void iniciarDiagnostico(Carro selectedCar) {
        try {
            // Create a new KieSession for diagnostic rules
            KieServices ks = KieServices.Factory.get();
            KieContainer kc = ks.getKieClasspathContainer();
            KieSession diagSession = kc.newKieSession("ksession-diagnostic");

            // Set the selected car as a global variable
            diagSession.setGlobal("selectedCar", selectedCar);

            // Create a new Resposta object for the diagnostic session
            Resposta diagResposta = new Resposta();
            diagResposta.setEstado("iniciarDiagnostico");
            diagResposta.setTexto("");

            FactHandle respostaHandle = diagSession.insert(diagResposta);

            Scanner scanner = new Scanner(System.in);

            while (!"finalizado".equals(diagResposta.getEstado())) {
                diagSession.fireAllRules();

                String estado = diagResposta.getEstado();
                String texto = diagResposta.getTexto();

                if (estado != null && estado.equals("perguntarSintoma") && (texto == null || texto.isEmpty())) {
                    System.out.print("Digite sua resposta: ");
                    String input = scanner.nextLine();
                    diagResposta.setTexto(input);

                    if (respostaHandle != null) {
                        diagSession.update(respostaHandle, diagResposta);
                    } else {
                        respostaHandle = diagSession.insert(diagResposta);
                    }
                }
            }

            diagSession.dispose();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
