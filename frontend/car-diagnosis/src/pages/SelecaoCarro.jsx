import { Box, Button, Card, CardContent, Container, FormControl, Grid, InputLabel, List, ListItem, MenuItem, Select, Typography } from "@mui/material";
import * as React from 'react';
import { useNavigate } from 'react-router-dom';
import Gauge from "../components/Gauge";
import TituloLinha from "../components/TituloLinha";
import { getSistemaSelecionado } from '../config/apiConfig';
import useSelecao from '../hooks/useSelecao';

const SelecaoCarro = () => {
  const sistemaSelecionado = getSistemaSelecionado();
  const navigate = useNavigate();
  const {
    marcas,
    marca,
    modelo,
    motor,
    modelos,
    motores,
    componentes,
    error,
    handleMarcaChange,
    handleModeloChange,
    handleMotorChange,
    iniciarDiagnostico,
  } = useSelecao();

  const tamanho_img = '280px';

  const getImagePath = () => {
    try {
      if (marca && modelo) {
        return require(`../assets/img/carros/${marca}/${modelo}.png`);
      }
    } catch (err) {
      console.error('Imagem nao encontrada', err);
    }

    return require(`../assets/img/carros/question-car.png`);
  };

  return (
    <Container sx={{ width: '1000px', margin: 'auto', padding: '20px'}}>
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="100vh">
        <Card sx={{ maxWidth: 900 }}>
          <CardContent padding={2}>
            <Typography variant="h4" component="h1" gutterBottom>
              Diagnóstico de Carro: {sistemaSelecionado}
            </Typography>
            <TituloLinha title="Selecione o Carro" lineColor="white" icon="DirectionsCar" position="13px" />
            <Grid 
              container 
              spacing={2} 
              padding={2}
              sx={{ width: '850px', margin: 'auto'}}
            >
              <Grid item xs={12} md={6} sx={{ width:tamanho_img, margin: 'auto'}}>
                <Box mt={2}>
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
                          <em>Selecione Marca</em>
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
                            <em>Selecione Modelo</em>
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
                            <em>Selecione Motor</em>
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
                <TituloLinha title="Níveis Ideais dos Componentes" lineColor="white" icon="Settings"/>
                <Box>
                  <Grid container spacing={2}>
                    {componentes.map((comp, idx) => (
                      <Grid item xs={12} md={3} key={idx}>
                          <Gauge
                            value={comp.valorMinimoIdeal}
                            min={comp.valorMinimo}
                            max={comp.valorMaximo}
                            max_ideal={comp.valorMaximoIdeal}
                            label={comp.nome}
                            units={comp.unidade}
                          />
                      </Grid>
                    ))}
                  </Grid>
                </Box>
              </Box>
            )}
            {marca && modelo && motor && componentes.length > 0 && (
              <><Box
                sx={{ height: '0.5px', background: 'white', marginBottom: '30px' }} />
                <Button
                  onClick={() => iniciarDiagnostico(navigate)}
                  color="primary"
                  variant="contained"
                  sx={{ width: '300px', height: '50px', fontSize: '1.2rem' }}
                >
                  Iniciar Diagnostico
                </Button></>
            )}
            {error && (
              <Typography variant="caption" color="error" gutterBottom sx={{ display: 'block' }}>
                {error}
              </Typography>
            )}
          </CardContent>
        </Card>
      </Box>
    </Container>
  );
};

export default SelecaoCarro;