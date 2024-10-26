// const API_URL = 'http://localhost:8080/api';

// export default API_URL;
let API_URL = 'http://localhost:8080/api';

export const setApiUrl = (url) => {
    API_URL = url;
};

export const getApiUrl = () => API_URL;

export default API_URL;
