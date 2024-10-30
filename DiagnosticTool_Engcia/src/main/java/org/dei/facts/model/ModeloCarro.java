package org.dei.facts.model;

import lombok.*;

import java.util.List;

/**
 * The ModeloCarro class represents a car model with a name.
 */
@Data
@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ModeloCarro {
    private String nome; // The name of the car model
}