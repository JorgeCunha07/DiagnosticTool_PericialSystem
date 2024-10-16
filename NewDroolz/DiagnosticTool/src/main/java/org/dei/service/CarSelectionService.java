package org.dei.service;

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

public class CarSelectionService {

    public Carro selecionarCarro(List<Carro> carros) {
        try {
            // Configurar KieServices e KieSession
            KieServices ks = KieServices.Factory.get();
            KieContainer kc = ks.getKieClasspathContainer();
            KieSession kSession = kc.newKieSession("ksession-rules");

            // Inicializa o objeto resposta
            Resposta resposta = new Resposta();
            resposta.setEstado(""); // Inicializa o estado com uma string vazia
            resposta.setTexto("");  // Inicializa o texto com uma string vazia

            // Definir variáveis globais
            kSession.setGlobal("carros", carros);
            kSession.setGlobal("triggeredRules", new ArrayList<String>());
            kSession.setGlobal("LOG", LoggerFactory.getLogger(CarSelectionService.class));

            // Inserir o objeto resposta na sessão
            FactHandle respostaHandle = kSession.insert(resposta);

            Scanner scanner = new Scanner(System.in);

            // Loop para processar as regras e interagir com o usuário até o diagnóstico ser finalizado
            while (!"finalizado".equals(resposta.getEstado())) {
                kSession.fireAllRules();

                String estado = resposta.getEstado();
                String texto = resposta.getTexto();

                // Pergunta ao usuário quando o sistema estiver aguardando uma resposta
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

            // Após a interação, retornar o carro selecionado
            Carro selectedCar = resposta.getCarroSelecionado();

            // Encerrar a sessão
            kSession.dispose();

            return selectedCar;

        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}
