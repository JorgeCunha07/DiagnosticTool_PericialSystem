package org.dei.service;

import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
import org.dei.facts.model.ModeloCarro;
import org.dei.facts.model.Motor;
import org.kie.api.KieServices;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.kie.api.runtime.rule.FactHandle;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class CarSelectionService {

    private KieSession kSession;
    private Resposta resposta;
    private FactHandle respostaHandle;

    public CarSelectionService() {
        // Configurar KieServices e KieSession
        KieServices ks = KieServices.Factory.get();
        KieContainer kc = ks.getKieClasspathContainer();
        this.kSession = kc.newKieSession("ksession-rules");

        // Inicializa o objeto resposta
        this.resposta = new Resposta();
        resposta.setEstado(""); // Inicializa o estado com uma string vazia
        resposta.setTexto("");  // Inicializa o texto com uma string vazia

        // Definir variáveis globais
        kSession.setGlobal("triggeredRules", new ArrayList<String>());
        kSession.setGlobal("LOG", LoggerFactory.getLogger(CarSelectionService.class));

        // Inserir o objeto resposta na sessão
        this.respostaHandle = kSession.insert(resposta);
    }

    public Resposta processarResposta(List<Carro> carros, String inputTexto) {
        // Definir a lista de carros como variável global
        kSession.setGlobal("carros", carros);

        // Atualiza o texto da resposta
        resposta.setTexto(inputTexto);

        // Atualiza o objeto resposta na sessão
        if (respostaHandle != null) {
            kSession.update(respostaHandle, resposta);
        } else {
            respostaHandle = kSession.insert(resposta);
        }

        // Processa as regras
        kSession.fireAllRules();

        if (resposta.getEstado().equals("finalizado")) {
            kSession.dispose();
            resposta.setCarroSelecionado(getCarroSelecionado(carros,resposta));
        }
        // Retorna a resposta atualizada
        return resposta;
    }

    public Carro getCarroSelecionado(List<Carro> carros, Resposta resposta) {
        for (Carro carro : carros) {
            if (carro.getMarca().getNome().equalsIgnoreCase(resposta.getMarcaSelecionada()) &&
                    carro.getModelo().getNome().equalsIgnoreCase(resposta.getModeloSelecionado()) &&
                    carro.getMotor().getNome().equalsIgnoreCase(resposta.getMotorSelecionado())) {
                return carro;
            }
        }
        return null;
    }
}
