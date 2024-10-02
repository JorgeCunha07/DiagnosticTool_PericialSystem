package org.innov8.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Hypothesis extends Fact {
    private String description;

    public Hypothesis(String description) {
        super();
        this.description = description;
    }

}
