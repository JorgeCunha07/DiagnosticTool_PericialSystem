package org.dei.facts.model;

import lombok.*;

import java.util.ArrayList;
import java.util.List;

/**
 * The Carro class represents a car with a specific brand, model, engine, and a list of components.
 */
@Data
@Getter
@Setter
@ToString
@NoArgsConstructor
public class Carro {
    private Marca marca; // The brand of the car
    private ModeloCarro modelo; // The model of the car
    private Motor motor; // The engine of the car
    private List<Componente> componentes; // The list of components of the car

    /**
     * Constructs a Carro instance with the specified brand, model, and engine.
     *
     * @param marca the brand of the car
     * @param modelo the model of the car
     * @param motor the engine of the car
     */
    public Carro(Marca marca, ModeloCarro modelo, Motor motor) {
        this.marca = marca;
        this.modelo = modelo;
        this.motor = motor;
        this.componentes = new ArrayList<>();
    }

    /**
     * Adds a component to the list of components of the car.
     *
     * @param componente the component to add
     */
    public void adicionarComponente(Componente componente) {
        this.componentes.add(componente);
    }
}