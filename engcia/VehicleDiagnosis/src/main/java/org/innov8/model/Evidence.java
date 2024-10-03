package org.innov8.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.lang.String;
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Evidence extends Fact{

    // TODO mudar nomes das variaveis para uppercase

    // O carro apresenta algum problema
    public static final String CAR_ISSUE = "O carro apresenta algum problema";

    // O carro não liga
    public static final String CAR_TURNS_ON = "O carro não liga";

    // Verificar a bateria (Bateria fraca ou sem carga?)
    public static final String BATTERY_CHECK = "Bateria fraca ou sem carga";

    // Verificar motor de arranque (Motor de arranque com defeito?)
    public static final String STARTING_MOTOR = "Motor de arranque com defeito";

    // Verificar sistema de ignição (Ignição a falhar?)
    public static final String IGNITION_SYSTEM = "Ignição a falhar";

    // Verificar sistema de segurança (Imobilizador ou alarme a bloquear?)
    public static final String SECURITY_SYSTEM = "Imobilizador ou alarme a bloquear";

    // Verificar sistema de segurança (Imobilizador ou alarme a bloquear?)
    public static final String ALARM_BLOCKED = "Imobilizador ou alarme a bloquear";

    // O carro liga mas vai abaixo?
    public static final String TURNS_ON_AND_SHUTSDOWN = "O carro liga mas vai abaixo";

    // Verificar superaquecimento do motor (Motor a sobreaquecer?)
    public static final String ENGINE_OVERHEATS = "Motor a sobreaquecer";

    // Verificar sistema de arrefecimento (Radiador ou ventoinha com defeito?)
    public static final String FAULTY_RADIATOR = "Radiador com defeito";

    public static final String FAULTY_FAN = "Ventoinha com defeito";

    // Verificar fugas no sistema de arrefecimento (Fuga detectado?)
    public static final String COOLING_SYSTEM_LEAKS = "Fuga no sistema de arrefecimento detectado";

    // Verificar termostato (Termostato defeituoso?)
    public static final String FAULTY_THERMOSTAT = "Termostato defeituoso";

    // Verificar sistema de combustível (Falta de combustível ou falha na bomba?)
    public static final String FUEL_SYSTEM_ISSUE = "Falta de combustível ou falha na bomba";

    // Verificar sistema de injeção de combustível (Injeção de combustível com defeito?)
    public static final String FUEL_INJECTION_SYSTEM = "Injeção de combustível com defeito";

    // Verificar sistema de ignição (Velas de ignição defeituosas?)
    public static final String FAULTY_SPARK_PLUG = "Velas de ignição defeituosas";

    private String evidence;
    private String value;

    public String toString() {
        return (evidence + " = " + value);
    }


}
