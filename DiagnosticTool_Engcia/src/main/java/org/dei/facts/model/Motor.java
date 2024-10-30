package org.dei.facts.model;

import lombok.*;

/**
 * The Motor class represents an engine with a name.
 */
@Data
@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class Motor {
    private String nome; // The name of the engine
}