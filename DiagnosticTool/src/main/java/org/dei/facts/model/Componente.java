package org.dei.facts.model;

import lombok.*;

@Data
@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class Componente {
    private String nome;
    private double valorMinimo;
    private double valorMaximo;
    private double valorMinimoIdeal;
    private double valorMaximoIdeal;
    private String unidade;
}
