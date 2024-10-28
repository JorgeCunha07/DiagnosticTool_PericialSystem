import { Alert, Box, Button, CircularProgress, Typography } from "@mui/material";
import axios from 'axios';
import { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import CardWrapper from '../components/CardWrapper';
import TituloLinha from "../components/TituloLinha";
import { getApiUrl } from '../config/apiConfig';

const useDiagnostico = (initialData) => {
  const [diagnostico, setDiagnostico] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const navigate = useNavigate();
  const API_URL = getApiUrl();

  const fetchPergunta = async () => {
    setLoading(true);
    try {
      const response = await axios.get(`${API_URL}/pergunta`);
      
      if (response.data.estado === "finalizado") {
        navigate('/conclusao/prolog');
      } else {
        setDiagnostico(response.data);
      }
    } catch (err) {
      setError('Falha ao carregar pergunta.');
    } finally {
      setLoading(false);
    }
  };

  const handleAnswer = async (answer) => {
    setLoading(true);
    try {
      const response = await axios.post(`${API_URL}/responder`, { resposta: answer });

      if (response.data.estado === "finalizado") {
        navigate('/conclusao/prolog');
      } else {
        fetchPergunta();
      }
    } catch (err) {
      setError('Falha ao enviar resposta.');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchPergunta();
  }, []);

  return { diagnostico, loading, error, handleAnswer };
};

const DiagnosticoCarroProlog = () => {
  const { diagnostico, loading, error, handleAnswer } = useDiagnostico();

  if (!diagnostico) return null;

  return (
    <CardWrapper titulo={`Questionário Diagnóstico`}>
      <TituloLinha title="Pergunta" lineColor="white" icon="QuestionMark" position="13px" />
      
      <Box sx={{ mt: 3 , display: 'flex', justifyContent: 'center', alignItems: 'center', whiteSpace:'pre-line', paddingBottom:'70px' }}>
        <Typography variant="h5" component="h2" sx={{ mt: 2 }}>
          {diagnostico.pergunta}
        </Typography>
      </Box>

      {loading && (
        <Box sx={{ display: 'flex', justifyContent: 'center', mt: 2 }}>
          <CircularProgress />
        </Box>
      )}

      {error && (
        <Box sx={{ mt: 2 }}>
          <Alert severity="error">{error}</Alert>
        </Box>
      )}
      
      {/* <TituloLinha title="Resposta" lineColor="white" icon="QuestionAnswer" position="13px" /> */}

      <Box sx={{ height: '0.5px', width: "100%", background: 'white', marginBottom: '30px'}} />

      {diagnostico.respostas && !loading && (
        <Box sx={{ mt: 3 , display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
          {diagnostico.respostas.map((resposta, index) => (
            <Button
              key={index}
              onClick={() => handleAnswer(resposta)}
              variant="contained"
              color="primary"
              sx={{ mr: 2 }}
            >
              {resposta}
            </Button>
          ))}
        </Box>
      )}
    </CardWrapper>
  );
};

export default DiagnosticoCarroProlog;
