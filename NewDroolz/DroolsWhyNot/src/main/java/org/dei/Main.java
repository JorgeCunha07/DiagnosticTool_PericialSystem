package org.dei;

import org.kie.api.KieServices;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
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

            // Configurar KieServices e KieSession
            KieServices ks = KieServices.Factory.get();
            KieContainer kc = ks.getKieClasspathContainer();
            KieSession kSession = kc.newKieSession("ksession-rules");

            // Definir variáveis globais
            kSession.setGlobal("carros", carros);
            kSession.setGlobal("triggeredRules", new ArrayList<String>());
            kSession.setGlobal("LOG", LoggerFactory.getLogger(Main.class));

            // Capturar respostas do usuário
            Scanner scanner = new Scanner(System.in);
            Resposta resposta = new Resposta();
            kSession.insert(resposta);

            while (true) {
                kSession.fireAllRules();
                if ("finalizado".equals(resposta.getTexto())) {
                    break;
                }
                System.out.println("Digite sua resposta:");
                String input = scanner.nextLine();
                kSession.delete(kSession.getFactHandle(resposta));
                resposta.setTexto(input);
                kSession.insert(resposta);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}