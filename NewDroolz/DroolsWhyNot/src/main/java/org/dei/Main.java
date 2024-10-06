package org.dei;

import org.kie.api.KieServices;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
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
            resposta.setEstado(""); // Initialize estado to an empty string
            resposta.setTexto("");  // Initialize texto to an empty string

            // Configurar KieServices e KieSession
            KieServices ks = KieServices.Factory.get();
            KieContainer kc = ks.getKieClasspathContainer();
            KieSession kSession = kc.newKieSession("ksession-rules");

            // Definir variáveis globais
            kSession.setGlobal("carros", carros);
            kSession.setGlobal("triggeredRules", new ArrayList<String>());
            kSession.setGlobal("LOG", LoggerFactory.getLogger(Main.class));

            // Capturar respostas do usuário
            // Exemplo simplificado
            Scanner scanner = new Scanner(System.in);
            kSession.insert(resposta);

            while (!"finalizado".equals(resposta.getEstado())) {
                kSession.fireAllRules();

                String estado = resposta.getEstado();
                String texto = resposta.getTexto();

                if (estado != null && estado.startsWith("aguardando") && (texto == null || texto.isEmpty())) {
                    System.out.print("Digite sua resposta: ");
                    String input = scanner.nextLine();
                    resposta.setTexto(input);

                    FactHandle respostaHandle = kSession.getFactHandle(resposta);
                    if (respostaHandle != null) {
                        kSession.update(respostaHandle, resposta);
                    } else {
                        kSession.insert(resposta);
                    }
                }
            }


        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}