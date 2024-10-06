package org.dei.facts;

public class Classif {
    private String id;
    private Values val;

    public Classif(String id, Values val) {
        this.id = id;
        this.val = val;
    }

    public String getId() {
        return id;
    }

    public Values getVal() {
        return val;
    }

    @Override
    public String toString() {
        return "Classif{" +
                "id='" + id + '\'' +
                ", val=" + val +
                '}';
    }
}
