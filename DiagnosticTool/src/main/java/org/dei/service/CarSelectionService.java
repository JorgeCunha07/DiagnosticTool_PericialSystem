package org.dei.service;

import org.dei.facts.Resposta;
import org.dei.facts.model.Carro;
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
        KieServices ks = KieServices.Factory.get();
        KieContainer kc = ks.getKieClasspathContainer();
        this.kSession = kc.newKieSession("ksession-rules");

        this.resposta = new Resposta();
        resposta.setEstado("");
        resposta.setTexto("");


        kSession.setGlobal("triggeredRules", new ArrayList<String>());
        kSession.setGlobal("LOG", LoggerFactory.getLogger(CarSelectionService.class));

        this.respostaHandle = kSession.insert(resposta);
    }

    public Resposta processarResposta(List<Carro> carros, String inputTexto) {

        kSession.setGlobal("carros", carros);

        resposta.setTexto(inputTexto);

        if (respostaHandle != null) {
            kSession.update(respostaHandle, resposta);
        } else {
            respostaHandle = kSession.insert(resposta);
        }

        kSession.fireAllRules();

        if (resposta.getEstado().equals("finalizado")) {
            kSession.dispose();
            resposta.setCarroSelecionado(getCarroSelecionado(carros,resposta));
        }
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
