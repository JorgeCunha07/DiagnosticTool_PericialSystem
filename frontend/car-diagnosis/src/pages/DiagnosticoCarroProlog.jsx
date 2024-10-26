import HomeIcon from '@mui/icons-material/Home';
import { Alert, Box, Button, Card, CardContent, CircularProgress, Container, IconButton, Typography } from "@mui/material";
import axios from 'axios';
import { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { getApiUrl, getSistemaSelecionado } from '../config/apiConfig';

//const sistemaSelecionado = getSistemaSelecionado();

const useDiagnostico = (initialData) => {
  const [carroSelecionado, setCarroSelecionado] = useState(initialData);
  const [diagnostico, setDiagnostico] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const navigate = useNavigate();
  const API_URL = getApiUrl();

  // console.log(">>>>> 1. initialData: " + initialData);
  // console.log(">>>>> 2. carroSelecionado: " + carroSelecionado);
  // console.log(">>>>> 3. diagnostico: " + diagnostico);

  const fetchPergunta = async () => {
    setLoading(true);
    try {
      const response = await axios.get(`${API_URL}/pergunta`);
      
      if (response.data.estado === "finalizado") {
        navigate('/conclusao');
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
        navigate('/conclusao');
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
  const navigate = useNavigate();
  const { diagnostico, loading, error, handleAnswer } = useDiagnostico();
  const sistemaSelecionado = getSistemaSelecionado();

  if (!diagnostico) return null;

  return (
    <Container>
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="100vh">
        <Card sx={{ position: 'relative', padding: 2 }}>
          <Box sx={{ position: 'absolute', top: 16, right: 16 }}>
            <IconButton
              variant="contained"
              onClick={() => navigate('/')}
              color='primary'
              sx={{ color: 'primary', backgroundColor: 'white' }}
            >
              <HomeIcon />
            </IconButton>
          </Box>
          <CardContent>
            <Typography variant="h4" component="h1" gutterBottom>
              Questionário Diagnóstico: {sistemaSelecionado}
            </Typography>

            <Typography variant="h6" component="h2" sx={{ mt: 2 }}>
              {diagnostico.pergunta}
            </Typography>

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

            {diagnostico.respostas && !loading && (
              <Box sx={{ mt: 3 }}>
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
          </CardContent>

          <CardContent>
            <Typography variant="body1" gutterBottom>
              JSON response:
            </Typography>
            <Box
              component="pre"
              sx={{
                whiteSpace: 'pre-wrap',
                wordWrap: 'break-word',
                backgroundColor: 'black',
                padding: '10px',
                borderRadius: '5px',
                overflowX: 'auto',
                maxHeight: '500px',
              }}
            >
              {JSON.stringify(diagnostico, null, 2)}
            </Box>
          </CardContent>
        </Card>
      </Box>
    </Container>
  );
};

export default DiagnosticoCarroProlog;
