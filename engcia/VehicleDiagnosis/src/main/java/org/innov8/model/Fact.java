package org.innov8.model;

import lombok.Getter;

@Getter
public class Fact {

    static private int lastId = 0;
    private int id;

    public Fact() {
        Fact.lastId ++;
        this.id = lastId;
    }

}
