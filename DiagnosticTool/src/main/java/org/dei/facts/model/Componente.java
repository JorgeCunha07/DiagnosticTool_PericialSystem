package org.dei.facts.model;

import lombok.*;

/**
 * The Componente class represents a component with a name, minimum and maximum values,
 * ideal minimum and maximum values, and a unit of measurement.
 */
@Data
@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class Componente {
    private String nome; // The name of the component
    private double valorMinimo; // The minimum value of the component
    private double valorMaximo; // The maximum value of the component
    private double valorMinimoIdeal; // The ideal minimum value of the component
    private double valorMaximoIdeal; // The ideal maximum value of the component
    private String unidade; // The unit of measurement for the component
}