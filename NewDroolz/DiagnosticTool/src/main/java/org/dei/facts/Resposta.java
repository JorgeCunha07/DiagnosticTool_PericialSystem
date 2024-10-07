package org.dei.facts;

import org.dei.facts.model.Carro;

public class Resposta {
    private String texto;              // User input
    private String estado;             // Conversation state
    private String marcaSelecionada;   // Selected marca
    private String modeloSelecionado;  // Selected modelo
    private String motorSelecionado;   // Selected motor
    private Carro carroSelecionado;    // Selected Carro object

    // Constructors, getters, and setters

    public String getTexto() {
        return texto;
    }

    public void setTexto(String texto) {
        this.texto = texto;
    }

    public String getEstado() {
        return estado;
    }

    public void setEstado(String estado) {
        this.estado = estado;
    }

    public String getMarcaSelecionada() {
        return marcaSelecionada;
    }

    public void setMarcaSelecionada(String marcaSelecionada) {
        this.marcaSelecionada = marcaSelecionada;
    }

    public String getModeloSelecionado() {
        return modeloSelecionado;
    }

    public void setModeloSelecionado(String modeloSelecionado) {
        this.modeloSelecionado = modeloSelecionado;
    }

    public String getMotorSelecionado() {
        return motorSelecionado;
    }

    public void setMotorSelecionado(String motorSelecionado) {
        this.motorSelecionado = motorSelecionado;
    }

    public Carro getCarroSelecionado() {
        return carroSelecionado;
    }

    public void setCarroSelecionado(Carro carroSelecionado) {
        this.carroSelecionado = carroSelecionado;
    }
}
