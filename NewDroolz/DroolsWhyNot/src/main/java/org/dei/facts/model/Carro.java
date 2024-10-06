package org.dei.facts.model;
import java.util.ArrayList;
import java.util.List;

public class Carro {
    private Marca marca;
    private ModeloCarro modelo;
    private Motor motor;
    private List<Componente> componentes;

    public Carro(Marca marca, ModeloCarro modelo, Motor motor) {
        this.marca = marca;
        this.modelo = modelo;
        this.motor = motor;
        this.componentes = new ArrayList<>();
    }

    public Carro() {
    }

    public void adicionarComponente(Componente componente) {
        this.componentes.add(componente);
    }


    public Marca getMarca() {
        return marca;
    }

    public void setMarca(Marca marca) {
        this.marca = marca;
    }

    public ModeloCarro getModelo() {
        return modelo;
    }

    public void setModelo(ModeloCarro modelo) {
        this.modelo = modelo;
    }

    public Motor getMotor() {
        return motor;
    }

    public void setMotor(Motor motor) {
        this.motor = motor;
    }

    public List<Componente> getComponentes() {
        return componentes;
    }

    public void setComponentes(List<Componente> componentes) {
        this.componentes = componentes;
    }

    @Override
    public String toString() {
        return "Carro{" +
                "marca=" + marca +
                ", modelo=" + modelo +
                ", motor=" + motor +
                ", componentes=" + componentes +
                '}';
    }
}
