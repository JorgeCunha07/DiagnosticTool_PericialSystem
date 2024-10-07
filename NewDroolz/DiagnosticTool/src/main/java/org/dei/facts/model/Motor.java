package org.dei.facts.model;

public class Motor {
    private String nome;

    public Motor(String nome) {
        this.nome = nome;
    }

    // Getters e Setters
    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    @Override
    public String toString() {
        return "Motor{" +
                "nome='" + nome + '\'' +
                '}';
    }
}
