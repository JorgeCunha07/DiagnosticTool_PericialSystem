import { Box, Button, FormControl, Grid, InputLabel, List, ListItem, MenuItem, Select, Typography } from "@mui/material";
import axios from 'axios';
import * as React from 'react';
import { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import CardWrapper from "../components/CardWrapper";
import Gauge from "../components/Gauge";
import TituloLinha from "../components/TituloLinha";
import { getApiUrl, getSistemaSelecionado } from '../config/apiConfig';

const SelecaoCarro = () => {
  const sistemaSelecionado = getSistemaSelecionado();
  const API_URL = getApiUrl();
  const navigate = useNavigate();

  const [carData, setCarData] = useState([]);
  const [marca, setMarca] = useState('');
  const [modelo, setModelo] = useState('');
  const [motor, setMotor] = useState('');
  const [modelos, setModelos] = useState([]);
  const [motores, setMotores] = useState([]);
  const [componentes, setComponentes] = useState([]);
  const [error, setError] = useState(null);

  const tamanho_img = '280px';

  useEffect(() => {
    const fetchCarData = async () => {
      try {
        const response = await axios.get(`${API_URL}/carros`);
        setCarData(response.data);
      } catch (error) {
        console.error(`Erro carregando dados da API ${sistemaSelecionado}`, error);
      }
    };
    fetchCarData();
  }, [API_URL]);

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

  const iniciarDiagnostico = async () => {
    let body;
    let url;
    let prolog_response;

    // PARA PROLOG
    if (sistemaSelecionado === "PROLOG") {

      // obterNumeroCarro
        url = `${API_URL}/obterNumeroCarro`; // http://localhost:4040/obterNumeroCarro
        body = {
            marca: marca ,
            modelo:  modelo ,
            motor: motor ,
            componentes: componentes,
        };

        try {
          
          // selecionarCarro
          prolog_response = await axios.post(url, body);
          const numeroCarro = prolog_response.data.numero;

          //console.log(">>>> numeroCarro: " + numeroCarro);

          url = `${API_URL}/selecionarCarro`; // http://localhost:4040/selecionarCarro
          body = {
            numero: numeroCarro,
          };


          prolog_response = await axios.post(url, body);
          //console.log(">>>> carro_escolhido: " + carro_escolhido);

          navigate('/diagnostico/prolog', { state: { diagnosticoData: prolog_response.data } });

        } catch (err) {
            setError('Falha ao iniciar diagnostico');
        }

    // PARA DROOLS
    } else if (sistemaSelecionado === "Drools") {
      
        url = `${API_URL}/diagnostico/iniciar`;
        body = {
            marca: { nome: marca },
            modelo: { nome: modelo },
            motor: { nome: motor },
            componentes: componentes,
        };

        try {
          const response = await axios.post(url, body);
          navigate('/diagnostico', { state: { diagnosticoData: response.data } });
        } catch (err) {
            setError(`Falha ao iniciar diagnostico: ${url}`);
        }

    } else {
        setError('Sistema não suportado');
        return;
    }
};

  const getImagePath = () => {
    try {
      if (marca && modelo) {
        return require(`../assets/img/carros/${marca}/${modelo}.png`);
      }
    } catch (err) {
      console.error('Imagem não encontrada', err);
    }
    return require(`../assets/img/carros/question-car.png`);
  };

  return (
    <CardWrapper titulo={`Diagnóstico de Carro`}>

      <TituloLinha title="Selecione o Carro" lineColor="white" icon="InfoTwoTone" position="13px" />

      <Grid container spacing={2} padding={2} sx={{ width: '850px', margin: 'auto'}}>
        <Grid item xs={12} md={6} sx={{ width:tamanho_img, margin: 'auto'}}>
          <Box mt={2} sx={{ display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
            <img src={getImagePath()} alt="Carro Selecionado" style={{ width:tamanho_img }} />
          </Box>
        </Grid>

        <Grid item xs={12} md={6} sx={{ height:'220px', margin: 'auto'}}>
          <List>
            <ListItem>
              <FormControl fullWidth>
                <InputLabel>Marca</InputLabel>
                <Select value={marca} onChange={handleMarcaChange}>
                  <MenuItem value="">
                    <em>Selecione a Marca</em>
                  </MenuItem>
                  {marcas.map((m, idx) => (
                    <MenuItem key={idx} value={m}>
                      {m}
                    </MenuItem>
                  ))}
                </Select>
              </FormControl>
            </ListItem>

            <ListItem>
              {modelos.length > 0 && (
                <FormControl fullWidth>
                  <InputLabel>Modelo</InputLabel>
                  <Select value={modelo} onChange={handleModeloChange}>
                    <MenuItem value="">
                      <em>Selecione o Modelo</em>
                    </MenuItem>
                    {modelos.map((m, idx) => (
                      <MenuItem key={idx} value={m}>
                        {m}
                      </MenuItem>
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
                    <MenuItem value="">
                      <em>Selecione o Motor</em>
                    </MenuItem>
                    {motores.map((m, idx) => (
                      <MenuItem key={idx} value={m}>
                        {m}
                      </MenuItem>
                    ))}
                  </Select>
                </FormControl>
                )}
              </ListItem>
            </List>
          </Grid>
        </Grid>

        {componentes.length > 0 && (
          <Box mt={2} padding={2}>

            <TituloLinha title="Níveis Ideais dos Componentes" lineColor="white" icon="BuildCircleTwoTone"/>

            <Box>
              <Grid container spacing={2}>
                {componentes.map((comp, idx) => (
                  <Grid item xs={12} md={3} key={idx}>
                    {/* Se valores Drools forem undefined (??), mostra os valores Prolog */}
                    <Gauge
                      //value={comp.valorMinimoIdeal ?? comp.minIdeal}
                      min={comp.valorMinimo ?? comp.min}
                      max={comp.valorMaximo ?? comp.max}
                      minIdeal={comp.valorMinimoIdeal ?? comp.minIdeal}
                      maxIdeal={comp.valorMaximoIdeal ?? comp.maxIdeal}
                      label={comp.nome.replace(/_/g, " ") ?? "N/A"} // substitui o "_" por " " (espaço)
                      units={comp.unidade ?? "N/A"}
                    />
                  </Grid>
                ))}
              </Grid>
            </Box>
          </Box>
        )}
          
        {marca && modelo && motor && componentes.length > 0 && (
          <>
            <Box sx={{ height: '0.5px', width: "100%", background: 'white', marginBottom: '30px'}} />

            <Box sx={{ display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
              <Button
                onClick={iniciarDiagnostico}
                color="primary"
                variant="contained"
                sx={{ width: '300px', height: '50px', fontSize: '1.2rem' }}
              >
                Iniciar Diagnostico
              </Button>
            </Box>
          </>
        )}
      {error && (
        <Typography variant="caption" color="error" gutterBottom sx={{ display: 'block' }}>
          {error}
        </Typography>
      )}
    </CardWrapper>
  );
};

export default SelecaoCarro;
