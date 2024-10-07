package org.dei.facts.model;

import lombok.*;

import java.util.List;

@Data
@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class Marca {
    private String nome;
    private List<ModeloCarro> modelos;
}
