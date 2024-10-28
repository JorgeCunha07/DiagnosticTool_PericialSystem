package org.dei.facts;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Evidence<T, V> {
    private String description;
    private T fact;
    private V value;

    @JsonCreator
    public Evidence(@JsonProperty("description") String description,
                    @JsonProperty("fact") T fact,
                    @JsonProperty("value") V value) {
        this.description = description;
        this.fact = fact;
        this.value = value;
    }

    @Override
    public String toString() {
        return "Evidence: Question -> " + fact + ", Answer -> " + value;
    }
}
