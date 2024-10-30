package org.dei.facts.model;

import lombok.*;

/**
 * The Marca class represents a car brand with a name.
 */
@Data
@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class Marca {
    private String nome; // The name of the car brand
}