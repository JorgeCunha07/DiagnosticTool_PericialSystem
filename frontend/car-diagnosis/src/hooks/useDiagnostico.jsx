import axios from 'axios';
import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import API_URL from '../config/apiConfig';

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
      marcaSelecionada: diagnostico.carroSelecionado.marca.nome,
      modeloSelecionado: diagnostico.carroSelecionado.modelo.nome,
      motorSelecionado: diagnostico.carroSelecionado.motor.nome,
      diagnostico: diagnostico.diagnostico,
      solucao: diagnostico.solucao,
      explicacaoGeral: diagnostico.explicacaoGeral,
      explicacaoGeralNao: diagnostico.explicacaoGeralNao,
      como: diagnostico.como,
      evidencias: diagnostico.evidencias,
      triggeredRules: diagnostico.triggeredRules
    };

    try {
      const response = await axios.post(`${API_URL}/diagnostico/responder`, requestBody);

      if (response.data.estado === 'finalizado') {
        navigate('/conclusao', { state: { responseData: response.data } });

      } else if (!response.data || !response.data.carroSelecionado.marca || !response.data.carroSelecionado.hasOwnProperty("marca")) {
        navigate('/error', { state: { responseData: response.data || 'Não recebeu dados válidos.' } });

      } else {
        setDiagnostico(response.data);
      }

    } catch (err) {
      setError('Falha ao enviar resposta.');
    } finally {
      setLoading(false);
    }
  };

  return { diagnostico, loading, error, handleAnswer };
};

export default useDiagnostico;
