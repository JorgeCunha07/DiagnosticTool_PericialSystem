package org.dei.facts;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Evidence<T, V> {
    private T arg0;
    private V arg1;

    // Constructor with JsonCreator annotation
    @JsonCreator
    public Evidence(@JsonProperty("arg0") T arg0, @JsonProperty("arg1") V arg1) {
        this.arg0 = arg0;
        this.arg1 = arg1;
    }

    // Getters and setters
    public T getArg0() {
        return arg0;
    }

    public void setArg0(T arg0) {
        this.arg0 = arg0;
    }

    public V getArg1() {
        return arg1;
    }

    public void setArg1(V arg1) {
        this.arg1 = arg1;
    }

    @Override
    public String toString() {
        return "Evidence: Question -> " + arg0 + ", Answer -> " + arg1;
    }
}