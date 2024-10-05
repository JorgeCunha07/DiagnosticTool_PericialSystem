package org.dei.facts;

public class Evidence<T> {
    private String id;
    private T val;

    public Evidence(String id, T val) {
        this.id = id;
        this.val = val;
    }

    public String getId() {
        return id;
    }

    public T getVal() {
        return val;
    }

    @Override
    public String toString() {
        return "Evidence{" +
                "id='" + id + '\'' +
                ", val=" + val +
                '}';
    }
}
