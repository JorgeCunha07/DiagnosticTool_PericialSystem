package org.dei.facts;

public class Evidence<T, V> {
    private T arg0;
    private V arg1;

    public Evidence(T arg0, V arg1) {
        this.arg0 = arg0;
        this.arg1 = arg1;
    }

    public T getArg0() {
        return arg0;
    }

    public V getArg1() {
        return arg1;
    }

    public void setArg0(T arg0) {
        this.arg0 = arg0;
    }

    public void setArg1(V arg1) {
        this.arg1 = arg1;
    }
}
