import { Box, Button, Card, CardActions, CardContent, Container, FormControl, InputLabel, List, ListItem, MenuItem, Select, Typography } from "@mui/material";
import axios from 'axios';
import * as React from 'react';
import { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom'; // Add this for navigation
import Gauge from "../components/Gauge";

const SelecaoCarro = () => {
  const [carData, setCarData] = useState([]);
  const [marca, setMarca] = useState('');
  const [modelo, setModelo] = useState('');
  const [motor, setMotor] = useState('');
  const [modelos, setModelos] = useState([]);
  const [motores, setMotores] = useState([]);
  const [componentes, setComponentes] = useState([]);
  const [error, setError] = useState(null);

  const navigate = useNavigate();  // Use this hook to navigate to another page

  const API_URL = 'http://localhost:8080/api';

  // Fetch data from API
  useEffect(() => {
    const fetchCarData = async () => {
      try {
        const response = await axios.get(API_URL + '/carros');
        setCarData(response.data);
      } catch (error) {
        console.error("Error fetching data from API", error);
      }
    };
    fetchCarData();
  }, []);

  // Get unique marcas
  const marcas = [...new Set(carData.map(car => car.marca.nome))];

  const handleMarcaChange = (e) => {
    const selectedMarca = e.target.value;
    setMarca(selectedMarca);
    setModelo('');
    setMotor('');

    const filteredModelos = carData.filter(car => car.marca.nome === selectedMarca);
    const uniqueModelos = [...new Set(filteredModelos.map(car => car.modelo.nome))];
    setModelos(uniqueModelos);
    setMotores([]);
    setComponentes([]);
  };

  const handleModeloChange = (e) => {
    const selectedModelo = e.target.value;
    setModelo(selectedModelo);
    setMotor('');

    const filteredMotores = carData.filter(car => car.marca.nome === marca && car.modelo.nome === selectedModelo);
    const uniqueMotores = [...new Set(filteredMotores.map(car => car.motor.nome))];
    setMotores(uniqueMotores);
    setComponentes([]);
  };

  const handleMotorChange = (e) => {
    const selectedMotor = e.target.value;
    setMotor(selectedMotor);

    const filteredCar = carData.find(car => car.marca.nome === marca && car.modelo.nome === modelo && car.motor.nome === selectedMotor);
    setComponentes(filteredCar ? filteredCar.componentes : []);
  };

  // POST request to initiate diagnostic
  const iniciarDiagnostico = async () => {
    const body = {
      marca: { nome: marca },
      modelo: { nome: modelo },
      motor: { nome: motor },
      componentes: componentes
    };

    try {
      const response = await axios.post(API_URL + '/diagnostico/iniciar', body);
      // Navigate to the diagnostic page and pass the response data
      navigate('/diagnostico', { state: { diagnosticoData: response.data } });
    } catch (err) {
      setError('Falha ao iniciar diagnostico');
    }
  };

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
            <Typography variant="h4" component="h1">
              Seleção do Carro
            </Typography>
            <List>
              <ListItem>
                <FormControl fullWidth>
                  <InputLabel>Marca</InputLabel>
                  <Select value={marca} onChange={handleMarcaChange}>
                    <MenuItem value=""><em>Selecione Marca</em></MenuItem>
                    {marcas.map((m, idx) => (
                      <MenuItem key={idx} value={m}>{m}</MenuItem>
                    ))}
                  </Select>
                </FormControl>
              </ListItem>

              <ListItem>
                {modelos.length > 0 && (
                  <FormControl fullWidth>
                    <InputLabel>Modelo</InputLabel>
                    <Select value={modelo} onChange={handleModeloChange}>
                      <MenuItem value=""><em>Selecione Modelo</em></MenuItem>
                      {modelos.map((m, idx) => (
                        <MenuItem key={idx} value={m}>{m}</MenuItem>
                      ))}
                    </Select>
                  </FormControl>
                )}
              </ListItem>

              <ListItem>
                {motores.length > 0 && (
                  <FormControl fullWidth>
                    <InputLabel>Motor</InputLabel>
                    <Select value={motor} onChange={handleMotorChange}>
                      <MenuItem value=""><em>Selecione Motor</em></MenuItem>
                      {motores.map((m, idx) => (
                        <MenuItem key={idx} value={m}>{m}</MenuItem>
                      ))}
                    </Select>
                  </FormControl>
                )}
              </ListItem>
            </List>

            {/* Display the image if Marca and Modelo are selected */}
            {marca && modelo && (
              <Box mt={2}>
                <img src={getImagePath()} alt="Selected Car or Placeholder" style={{ width: '300px' }} />
              </Box>
            )}

            {componentes.length > 0 && (
              <Box mt={2}>
                <Typography variant="h5" component="h2">Componentes</Typography>
                <List>
                  {componentes.map((comp, idx) => (
                    <ListItem key={idx}>
                      <Gauge
                        value={comp.valorMinimoIdeal}
                        min={comp.valorMinimo}
                        max={comp.valorMaximo}
                        max_ideal={comp.valorMaximoIdeal}
                        label={comp.nome}
                        units={comp.unidade}
                      />
                      <strong>{comp.nome}</strong>: {comp.valorMinimoIdeal} - {comp.valorMaximoIdeal} {comp.unidade}
                    </ListItem>
                  ))}
                </List>
              </Box>
            )}
          </CardContent>
          <CardActions>
            {marca && modelo && motor && componentes.length > 0 && (
              <Button onClick={iniciarDiagnostico} color="primary" variant="contained">
                Iniciar Diagnostico
              </Button>
            )}
            {error && (
              <Typography variant="caption" color="error" gutterBottom sx={{ display: 'block' }}>
                {error}
              </Typography>
            )}
          </CardActions>
        </Card>
      </Box>
    </Container>
  );
};

export default SelecaoCarro;
