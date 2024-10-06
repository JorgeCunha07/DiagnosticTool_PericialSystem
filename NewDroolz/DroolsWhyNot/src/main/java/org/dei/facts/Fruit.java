package org.dei.facts;

public class Fruit {
    private Values id;

    public Fruit(Values id) {
        this.id = id;
    }

    public Values getId() {
        return id;
    }

    @Override
    public String toString() {
        return "Fruit{" +
                "id=" + id +
                '}';
    }
}
