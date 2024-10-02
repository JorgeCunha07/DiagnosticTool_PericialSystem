package org.innov8.model;

import lombok.Getter;
import lombok.Setter;
import org.innov8.vehicleDiagnosis.VehicleDiagnosis;

@Getter
@Setter
public class Hypothesis extends Fact {

    private String description;
    private String value;

    public Hypothesis(String description, String value) {
        this.description = description;
        this.value = value;
        VehicleDiagnosis.agendaEventListener.addRhs(this);
    }

    public String toString() {
        return (description + " = " + value);
    }
}