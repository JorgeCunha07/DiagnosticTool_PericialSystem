// const API_URL = 'http://localhost:8080/api';

// export default API_URL;
// let API_URL = 'http://localhost:8080/api';

// export const setApiUrl = (url) => {
//     API_URL = url;
// };

// export const getApiUrl = () => API_URL;

// export default API_URL;

let API_URL = 'http://localhost:8080/api';
let sistemaSelecionado = 'Drools';

export const setApiUrl = (identifier) => {
    sistemaSelecionado = identifier;

    if (sistemaSelecionado === "Drools") {
        API_URL = 'http://localhost:8080/api'; // Sistema for Drools
    
    } else if (sistemaSelecionado === "PROLOG") { // Sistema for PROLOG
        API_URL = 'http://localhost:4040';
    } else {
        throw new Error("Sistema não é suportado: " + sistemaSelecionado);
    }
};

export const getApiUrl = () => API_URL;
export const getSistemaSelecionado = () => sistemaSelecionado;

export default API_URL;
