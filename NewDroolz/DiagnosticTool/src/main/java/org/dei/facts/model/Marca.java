package org.dei.facts.model;

import java.util.List;

public class Marca {
    private String nome;
    private List<ModeloCarro> modelos;

    public Marca(String nome, List<ModeloCarro> modelos) {
        this.nome = nome;
        this.modelos = modelos;
    }

    public Marca() {
    }

    // Getters e Setters
    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public List<ModeloCarro> getModelos() {
        return modelos;
    }

    public void setModelos(List<ModeloCarro> modelos) {
        this.modelos = modelos;
    }

    @Override
    public String toString() {
        return "Marca{" +
                "nome='" + nome + '\'' +
                ", modelos=" + modelos +
                '}';
    }
}
