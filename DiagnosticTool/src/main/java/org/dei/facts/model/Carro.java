package org.dei.facts.model;

import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Data
@Getter
@Setter
@ToString
@NoArgsConstructor
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

    public void adicionarComponente(Componente componente) {
        this.componentes.add(componente);
    }

}
