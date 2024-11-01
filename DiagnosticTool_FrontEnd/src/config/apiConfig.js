

// para correr numa rede local trocar o valor da variavel 'host' pelo resultado de:
// ifconfig | grep "inet " | grep -v 127.0.0.1
const host = 'localhost';

let API_URL = `http://${host}:8080/api`;
let sistemaSelecionado = 'Drools';

export const setApiUrl = (identifier) => {
    sistemaSelecionado = identifier;

    if (sistemaSelecionado === "Drools") {
        API_URL = `http://${host}:8080/api`; // Sistema for Drools
    
    } else if (sistemaSelecionado === "PROLOG") { // Sistema for PROLOG
        API_URL = `http://${host}:4040`;
    } else {
        throw new Error("Sistema não é suportado: " + sistemaSelecionado);
    }
};

export const getApiUrl = () => API_URL;
export const getSistemaSelecionado = () => sistemaSelecionado;

export default API_URL;
