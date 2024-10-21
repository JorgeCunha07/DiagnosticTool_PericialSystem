import { Alert, Box, Button, Card, CardContent, CircularProgress, Container, Typography } from "@mui/material";
import axios from 'axios';
import React, { useState } from 'react';
import { useLocation } from 'react-router-dom';

const API_URL = 'http://localhost:8080/api';

const DiagnosticoCarro = () => {
  const location = useLocation();
  const { diagnosticoData } = location.state; // Get the data passed from the previous page
  const [diagnostico, setDiagnostico] = useState(diagnosticoData); // Initialize with passed data
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  if (!diagnosticoData) return null;

  // Function to handle the button click and send the POST request
  const handleAnswer = async (answer) => {
    setLoading(true); // Show loading while the request is being processed


    const requestBody = {
      texto: answer,
      estado: diagnostico.estado,
      pergunta: diagnostico.pergunta,
      carroSelecionado: diagnostico.carroSelecionado,
    };

    try {
      const response = await axios.post(`${API_URL}/diagnostico/responder`, requestBody);
      
      // Update state with new diagnostic data
      setDiagnostico(response.data);
    } catch (err) {
      setError('Failed to send the response.');
    } finally {
      setLoading(false); // Hide loading when the request is complete
    }
  };

  const marca = diagnostico.carroSelecionado.marca.nome;
  const modelo = diagnostico.carroSelecionado.modelo.nome;
  const motor = diagnostico.carroSelecionado.motor.nome;

  const getImagePath = () => {
    try {
      // If both Marca and Modelo are selected, return the car image
      if (marca && modelo) {
        return require(`../assets/img/carros/${marca}/${modelo}.png`);
      }
    } catch (err) {
      console.error('Image not found, displaying placeholder', err);
    }

    // Return placeholder by default
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

      {/* Display car information */}
      <Typography variant="h6" component="h2">
      <Box mt={2}>
                <img src={getImagePath()} alt="Selected Car or Placeholder" style={{ width: '300px' }} />
              </Box>{marca}, {modelo}, {motor}
      </Typography>

      {/* Display the diagnostic question */}
      <Typography variant="h4" component="h3" sx={{ mt: 2 }}>
       {diagnostico.pergunta}
      </Typography>

      {/* Loading indicator */}
      {loading && (
        <Box sx={{ display: 'flex', justifyContent: 'center', mt: 2 }}>
          <CircularProgress />
        </Box>
      )}

      {/* Error message */}
      {error && (
        <Box sx={{ mt: 2 }}>
          <Alert severity="error">{error}</Alert>
        </Box>
      )}

      {/* Show Sim/Não buttons only if the question includes "(Sim/Não)" and not loading */}
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
