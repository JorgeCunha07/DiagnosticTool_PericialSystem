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

    // O carro apresenta algum problema
    private String carIssue;

    // O carro não liga
    private String carTurnsOn;

    // Verificar a bateria (Bateria fraca ou sem carga?)
    private String batteryCheck;

    // Verificar motor de arranque (Motor de arranque com defeito?)
    private String startingMotor;

    // Verificar sistema de ignição (Ignição a falhar?)
    private String ignitionSystem;

    // Verificar sistema de segurança (Imobilizador ou alarme a bloquear?)
    private String securitySystem;

    // Verificar sistema de segurança (Imobilizador ou alarme a bloquear?)
    private String alarmBlocked;

    // O carro liga mas vai abaixo?
    private String turnsOnAndShutsdown;

    // Verificar superaquecimento do motor (Motor a sobreaquecer?)
    private String engineOverheats;

    // Verificar sistema de arrefecimento (Radiador ou ventoinha com defeito?)
    private String faultyRadiator;

    private String faultyFan;

    // Verificar fugas no sistema de arrefecimento (Fuga detectado?)
    private String coolingSystemLeaks;

    // Verificar termostato (Termostato defeituoso?)
    private String faultyThermostat;

    // Verificar sistema de combustível (Falta de combustível ou falha na bomba?)
    private String fuelSystemIssue;

    // Verificar sistema de injeção de combustível (Injeção de combustível com defeito?)
    private String fuelInjectionSystem;

    // Verificar sistema de ignição (Velas de ignição defeituosas?)
    private String faultySparkPlug;

    private String evidence;
    private String value;

    public Evidence(String ev, String v) {
        evidence = ev;
        value = v;
    }

}
