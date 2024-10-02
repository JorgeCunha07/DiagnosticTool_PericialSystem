package org.innov8.model;

import java.lang.String;

public class Evidences {

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
    //              :Diagnóstico: Falha nas velas de ignição;
    //              :Solução: Substituir velas de ignição;
    private String faulty;
    public Evidences() {
    }

    public String getCarIssue() {
        return this.carIssue;
    }

    public void setCarIssue(String carIssue) {
        this.carIssue = carIssue;
    }

    public String getCarTurnsOn() {
        return this.carTurnsOn;
    }

    public void setCarTurnsOn(String carTurnsOn) {
        this.carTurnsOn = carTurnsOn;
    }

    public String getBatteryCheck() {
        return this.batteryCheck;
    }

    public void setBatteryCheck(String batteryCheck) {
        this.batteryCheck = batteryCheck;
    }

    public String getStartingMotor()
    {
        return this.startingMotor;
    }

    public void setStartingMotor(String startingMotor) {
        this.startingMotor = startingMotor;
    }

    public String getIgnitionSystem() {
        return this.ignitionSystem;
    }

    public void setIgnitionSystem(String ignitionSystem) {
        this.ignitionSystem = ignitionSystem;
    }

    public String getSecuritySystem() {
        return this.securitySystem;
    }

    public void setSecuritySystem(String securitySystem) {
        this.securitySystem = securitySystem;
    }

    public String getTurnsOnAndShutsdown() {
        return this.turnsOnAndShutsdown;
    }

    public void setTurnsOnAndShutsdown(String turnsOnAndShutsdown) {
        this.turnsOnAndShutsdown = turnsOnAndShutsdown;
    }

    public String getEngineOverheats()	{
        return this.engineOverheats;
    }

    public void setEngineOverheats(String engineOverheats) {
        this.engineOverheats = engineOverheats;
    }

    public String getFaultyRadiator() {
        return this.faultyRadiator;
    }

    public void setFaultyRadiator(String faultyRadiator) {
        this.faultyRadiator = faultyRadiator;
    }

    public String getCoolingSystemLeaks() {
        return this.coolingSystemLeaks;
    }

    public void setCoolingSystemLeaks(String coolingSystemLeaks) {
        this.coolingSystemLeaks = coolingSystemLeaks;
    }

    public String getFaultyThermostat() {
        return this.faultyThermostat;
    }

    public void setFaultyThermostat(String faultyThermostat) {
        this.faultyThermostat = faultyThermostat;
    }

    public String getFuelSystemIssue() {
        return this.fuelSystemIssue;
    }

    public void setFuelSystemIssue(String fuelSystemIssue) {
        this.fuelSystemIssue = fuelSystemIssue;
    }

    public String getFuelInjectionSystem() {
        return this.fuelInjectionSystem;
    }

    public void setFuelInjectionSystem(String fuelInjectionSystem) {
        this.fuelInjectionSystem = fuelInjectionSystem;
    }

    public Evidences(String carIssue, String earAche,
                     String batteryCheck, String startingMotor,
                     String ignitionSystem, String securitySystem,
                     String alarmBlocked,
                     String turnsOnAndShutsdown, String engineOverheats,
                     String faultyRadiator, String faultyFan,
                     String coolingSystemLeaks,
                     String bloodCoffee, String fuelSystemIssue,
                     String fuelInjectionSystem) {
        this.carIssue = carIssue;
        this.carTurnsOn = earAche;
        this.batteryCheck = batteryCheck;
        this.startingMotor = startingMotor;
        this.ignitionSystem = ignitionSystem;
        this.securitySystem = securitySystem;
        this.alarmBlocked = alarmBlocked;
        this.turnsOnAndShutsdown = turnsOnAndShutsdown;
        this.engineOverheats = engineOverheats;
        this.faultyRadiator = faultyRadiator;
        this.faultyFan = faultyFan;
        this.coolingSystemLeaks = coolingSystemLeaks;
        this.faultyThermostat = bloodCoffee;
        this.fuelSystemIssue = fuelSystemIssue;
        this.fuelInjectionSystem = fuelInjectionSystem;
    }

    public String getAlarmBlocked() {
        return alarmBlocked;
    }

    public void setAlarmBlocked(String alarmBlocked) {
        this.alarmBlocked = alarmBlocked;
    }

    public String getFaultyFan() {
        return faultyFan;
    }

    public void setFaultyFan(String faultyFan) {
        this.faultyFan = faultyFan;
    }
}
