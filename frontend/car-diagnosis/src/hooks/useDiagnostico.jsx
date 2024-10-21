import axios from 'axios';
import { useState } from 'react';
import { useNavigate } from 'react-router-dom';

const API_URL = 'http://localhost:8080/api';

const useDiagnostico = (initialData) => {
  const [diagnostico, setDiagnostico] = useState(initialData);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const navigate = useNavigate();

  const handleAnswer = async (answer) => {
    setLoading(true);

    const requestBody = {
      texto: answer,
      estado: diagnostico.estado,
      pergunta: diagnostico.pergunta,
      carroSelecionado: diagnostico.carroSelecionado,
    };

    try {
      const response = await axios.post(`${API_URL}/diagnostico/responder`, requestBody);

      if (!response.data || !response.data.carroSelecionado.marca || !response.data.carroSelecionado.hasOwnProperty("marca")) {
        navigate('/error', { state: { responseData: response.data || 'Não recebeu dados válidos.' } });
      } else {
        setDiagnostico(response.data);
      }
    } catch (err) {
      setError('Falha ao enviar resposta.');
    } finally {
      setLoading(false); // Hide loading when the request is complete
    }
  };

  return { diagnostico, loading, error, handleAnswer };
};

export default useDiagnostico;
