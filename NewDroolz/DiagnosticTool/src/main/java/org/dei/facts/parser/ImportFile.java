package org.dei.facts.parser;

import org.dei.facts.model.*;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class ImportFile {

    public static List<Carro> carregarBaseDados(String caminhoArquivo) {
        List<Carro> carros = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(caminhoArquivo))) {
            br.readLine();
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
        double valorMinimoIdeal = Double.parseDouble(valores[4].trim());
        double valorMaximoIdeal = Double.parseDouble(valores[5].trim());
        double valorMinimo = 0;
        double valorMaximo = Double.parseDouble(valores[6].trim());
        String unidade = valores[7].trim();

        Marca marca = new Marca(nomeMarca);
        ModeloCarro modeloCarro = new ModeloCarro(nomeModelo);
        Motor motor = new Motor(nomeMotor);
        Carro carro = obterOuCriarCarro(marca, modeloCarro, motor, carros);

        Componente componente = new Componente(nomeComponente, valorMinimo, valorMaximo, valorMinimoIdeal, valorMaximoIdeal, unidade);
        carro.adicionarComponente(componente);
    }

    private static Carro obterOuCriarCarro(Marca marca, ModeloCarro modeloCarro, Motor motor, List<Carro> carros) {
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