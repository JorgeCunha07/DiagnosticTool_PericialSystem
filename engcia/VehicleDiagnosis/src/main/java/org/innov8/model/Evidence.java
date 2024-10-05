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
    public static final String PROBLEMA_CARRO = "O carro apresenta algum problema";

    // O carro não liga
    public static final String CARRO_NAO_LIGA = "O carro não liga";

    // Verificar a bateria (Bateria fraca ou sem carga?)
    public static final String VERIFICAR_BATERIA = "Bateria fraca ou sem carga";

    // Verificar motor de arranque (Motor de arranque com defeito?)
    public static final String MOTOR_ARRANQUE_DEFEITO = "Motor de arranque com defeito";

    // Verificar sistema de ignição (Ignição a falhar?)
    public static final String IGNICAO_FALHA = "Ignição a falhar";

    // Verificar sistema de segurança (Imobilizador ou alarme a bloquear?)
    // Imobilizador
    public static final String IMOBILIZADOR_BLOQUEADO = "Imobilizador a bloquear";

    // Verificar sistema de segurança (Imobilizador ou alarme a bloquear?)
    // Alarme
    public static final String ALARME_BLOQUEADO = "Alarme a bloquear";

    // O carro liga, mas vai abaixo?
    public static final String CARRO_LIGA_DESLIGA = "O carro liga mas vai abaixo";

    // Verificar superaquecimento do motor (Motor a sobreaquecer?)
    public static final String MOTOR_SOBREAQUECIDO = "Motor a sobreaquecer";

    // Verificar sistema de arrefecimento (Radiador ou ventoinha com defeito?)
    public static final String RADIADOR_DEFEITUOSO = "Radiador com defeito";

    public static final String VENTOINHA_DEFEITUOSA = "Ventoinha com defeito";

    // Verificar fugas no sistema de arrefecimento (Fuga detectado?)
    public static final String ARREFECIMENTO_FUGAS = "Fuga no sistema de arrefecimento detectado";

    // Verificar termostato (Termostato defeituoso?)
    public static final String TERMOSTATO_DEFEITUOSO = "Termostato defeituoso";

    // Verificar sistema de combustível (Falta de combustível ou falha na bomba?)
    public static final String SISTEMA_COMBUSTIVEL_DEFEITUOSO = "Falta de combustível ou falha na bomba";

    // Verificar sistema de injeção de combustível (Injeção de combustível com defeito?)
    public static final String INJECAO_COMBUSTIVEL_DEFEITUOSA = "Injeção de combustível com defeito";

    // Verificar sistema de ignição (Velas de ignição defeituosas?)
    public static final String VELAS_DEFEITUOSAS = "Velas de ignição defeituosas";

    private String evidence;
    private String value;

    public String toString() {
        return (evidence + " = " + value);
    }


}
