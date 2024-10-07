package org.dei.facts.model;

import lombok.*;

import java.util.List;


@Data
@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ModeloCarro {
    private String nome;
    private List<Motor> motores;

}
