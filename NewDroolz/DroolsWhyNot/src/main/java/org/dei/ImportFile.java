package org.dei;

import org.dei.facts.model.*;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class ImportFile {

    // Load data from CSV
    static List<Carro> carregarBaseDados(String caminhoArquivo) {
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
