import { Alert, Box, Button, Card, CardContent, CircularProgress, Container, Typography } from "@mui/material";
import axios from 'axios';
import React, { useState } from 'react';
import { useLocation, useNavigate } from 'react-router-dom';

const API_URL = 'http://localhost:8080/api';

const DiagnosticoCarro = () => {
  const location = useLocation();
  const { diagnosticoData } = location.state;
  const [diagnostico, setDiagnostico] = useState(diagnosticoData);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const navigate = useNavigate();

  if (!diagnosticoData) return null;

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

  const marca = diagnostico.carroSelecionado.marca.nome;
  const modelo = diagnostico.carroSelecionado.modelo.nome;
  const motor = diagnostico.carroSelecionado.motor.nome;

  const getImagePath = () => {
    try {
      if (marca && modelo) {
        return require(`../assets/img/carros/${marca}/${modelo}.png`);
      }
    } catch (err) {
      console.error('Image not found, displaying placeholder', err);
    }
    return require(`../assets/img/carros/question-car.png`);
  };

  return (
    <Container>
      <Box
        display="flex"
        justifyContent="center"
        alignItems="center"
        minHeight="100vh"
      >
        <Card lg={{ minWidth: 600 }}>
          <CardContent>
      <Typography variant="h4" component="h1" gutterBottom>
        Questionario Diagnostico
      </Typography>

      <Typography variant="h6" component="h2">
      <Box mt={2}>
                <img src={getImagePath()} alt="Selected Car or Placeholder" style={{ width: '300px' }} />
              </Box>{marca}, {modelo}, {motor}
      </Typography>

      <Typography variant="h4" component="h3" sx={{ mt: 2 }}>
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

      {diagnostico.pergunta.includes("(Sim/Não)") && !loading && (
        <Box sx={{ mt: 3 }}>
          <Button 
            onClick={() => handleAnswer("Sim")}
            variant="contained"
            color="primary"
            sx={{ mr: 2 }}>
            Sim
          </Button>
          <Button 
            onClick={() => handleAnswer("Não")}
            variant="contained"
            color="secondary">
            Não
          </Button>
        </Box>
      )}
          </CardContent>
        </Card>
      </Box>
    </Container>
  );
};

export default DiagnosticoCarro;
