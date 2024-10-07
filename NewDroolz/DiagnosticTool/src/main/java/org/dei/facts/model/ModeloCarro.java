package org.dei.facts.model;

import java.util.List;

public class ModeloCarro {
    private String nome;
    private List<Motor> motores;

    public ModeloCarro(String nome, List<Motor> motores) {
        this.nome = nome;
        this.motores = motores;
    }

    public ModeloCarro() {

    }

    // Getters e Setters
    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public List<Motor> getMotores() {
        return motores;
    }

    public void setMotores(List<Motor> motores) {
        this.motores = motores;
    }

    @Override
    public String toString() {
        return "ModeloCarro{" +
                "nome='" + nome + '\'' +
                ", motores=" + motores +
                '}';
    }
}
