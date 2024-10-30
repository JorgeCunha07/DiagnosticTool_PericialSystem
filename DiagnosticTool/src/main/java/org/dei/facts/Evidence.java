package org.dei.facts;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

/**
 * The Evidence class represents a piece of evidence with a description, fact, and value.
 *
 * @param <T> the type of the fact
 * @param <V> the type of the value
 */
@Getter
@Setter
public class Evidence<T, V> {
    private String description; // Description of the evidence
    private T fact; // The fact associated with the evidence
    private V value; // The value of the fact

    /**
     * Constructor for the Evidence class.
     *
     * @param description the description of the evidence
     * @param fact the fact associated with the evidence
     * @param value the value of the fact
     */
    @JsonCreator
    public Evidence(@JsonProperty("description") String description,
                    @JsonProperty("fact") T fact,
                    @JsonProperty("value") V value) {
        this.description = description;
        this.fact = fact;
        this.value = value;
    }

    /**
     * Returns a string representation of the evidence.
     *
     * @return a string in the format "Evidence: Question -> fact, Answer -> value"
     */
    @Override
    public String toString() {
        return "Evidence: Question -> " + fact + ", Answer -> " + value;
    }
}