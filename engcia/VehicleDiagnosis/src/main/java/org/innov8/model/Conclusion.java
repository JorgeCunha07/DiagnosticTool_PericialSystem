package org.innov8.model;

import lombok.Getter;
import lombok.Setter;
import org.innov8.vehicleDiagnosis.VehicleDiagnosis;

@Setter
@Getter
public class Conclusion extends Fact {

    // Diagnóstico: Bateria com defeito ou sem carga;
    // Solução: Substituir ou recarregar a bateria;
    public static final String FAULTY_BATTERY_OR_NO_CHARGE = "Bateria com defeito ou sem carga | Substituir ou recarregar a bateria";

    // Diagnóstico: Falha no motor de arranque;
    // Solução: Substituir motor de arranque;
    public static final String FAULTY_STARTING_MOTOR = "Falha no motor de arranque | Substituir motor de arranque";

    // Diagnóstico: Sistema de ignição com defeito;
    // Solução: Verificar chave de ignição, fusíveis e conectores;
    public static final String FAULTY_IGNITION_SYSTEM = "Sistema de ignição com defeito | Verificar chave de ignição, fusíveis e conectores";

    // Diagnóstico: Sistema de segurança ativado;
    // Solução: Desativar sistema de segurança;
    public static final String SECURITY_SYSTEM_ACTIVATED = "Sistema de segurança ativado | Desativar sistema de segurança";
    
    // Diagnóstico: Problema no radiador ou ventoinha;
    // Solução: Substituir ou reparar radiador/ventoinha;
    public static final String FAULTY_RADIATOR_OR_FAN = "Problema no radiador ou ventoinha | Substituir ou reparar radiador/ventoinha";

    // Diagnóstico: Fugas no sistema de arrefecimento;
    // Solução: Reparar Fuga e completar fluido;
    public static final String COOLING_SYSTEM_LEAKING = "Fugas no sistema de arrefecimento | Reparar Fuga e completar fluido";

    // Diagnóstico: Termostato com defeito;
    // Solução: Substituir termostato;
    public static final String FAULTY_THERMOSTAT = "Termostato com defeito | Substituir termostato";

    // Diagnóstico: Falta de combustível ou bomba de combustível com defeito;
    // Solução: Reabastecer ou substituir a bomba de combustível;
    public static final String FAULTY_FUEL_SYSTEM = "Falta de combustível ou bomba de combustível com defeito | Reabastecer ou substituir a bomba de combustível";

    // Diagnóstico: Injeção de combustível com defeito;
    // Solução: Limpar ou substituir injetores;
    public static final String FAULTY_INJECTION_SYSTEM = "Injeção de combustível com defeito | Limpar ou substituir injetores";

    // Diagnóstico: Verificação adicional necessária;
    // Solução: Consultar especialista;
    public static final String UNKNOWN = "Problema indeterminado. Verificação adicional necessária. | Consultar especialista.";


    private String description;

    public Conclusion(String description) {
        this.description = description;
        VehicleDiagnosis.agendaEventListener.addRhs(this);
    }

    public String toString() {
        return "Diagnosis | Solution: " + description;
    }

}
