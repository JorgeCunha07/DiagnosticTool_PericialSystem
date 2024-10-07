package org.dei.facts.model;

public class Componente {
    private String nome;
    private double valorMinimo;
    private double valorMaximo;

    public Componente(String nome, double valorMinimo, double valorMaximo) {
        this.nome = nome;
        this.valorMinimo = valorMinimo;
        this.valorMaximo = valorMaximo;
    }

    // Getters e Setters
    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getValorMinimo() {
        return valorMinimo;
    }

    public void setValorMinimo(double valorMinimo) {
        this.valorMinimo = valorMinimo;
    }

    public double getValorMaximo() {
        return valorMaximo;
    }

    public void setValorMaximo(double valorMaximo) {
        this.valorMaximo = valorMaximo;
    }

    @Override
    public String toString() {
        return "Componente{" +
                "nome='" + nome + '\'' +
                ", valorMinimo=" + valorMinimo +
                ", valorMaximo=" + valorMaximo +
                '}';
    }
}
