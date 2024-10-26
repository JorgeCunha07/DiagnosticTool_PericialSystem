import HomeIcon from '@mui/icons-material/Home';
import { Alert, Box, Button, Card, CardContent, CircularProgress, Container, IconButton, TextField, Typography } from "@mui/material";
import React, { useState } from 'react';
import { useLocation, useNavigate } from 'react-router-dom';
import { getSistemaSelecionado } from '../config/apiConfig';
import useDiagnostico from '../hooks/useDiagnostico';

const DiagnosticoCarro = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const sistemaSelecionado = getSistemaSelecionado();

  const [nivelOleo, setNivelOleo] = useState(0.0);
  
  const { diagnosticoData } = location.state;
  const { diagnostico, loading, error, handleAnswer } = useDiagnostico(diagnosticoData);

  if (!diagnosticoData) return null;

  const marca = diagnostico.carroSelecionado.marca.nome;
  const modelo = diagnostico.carroSelecionado.modelo.nome;
  const motor = diagnostico.carroSelecionado.motor.nome;

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
    <Container>
      <Box
        display="flex"
        justifyContent="center"
        alignItems="center"
        minHeight="100vh"
      >
        <Card sx={{ position: 'relative', padding: 2 }}>
          <Box sx={{ position: 'absolute', top: 16, right: 16}}>
            <IconButton
              variant="contained"
              onClick={()=>navigate('/')}
              color='primary'
              sx={{ color: 'primary', backgroundColor: 'white'}}
            ><HomeIcon /></IconButton>
          </Box>
          <CardContent>
            <Typography variant="h4" component="h1" gutterBottom>
              Questionário Diagnóstico: {sistemaSelecionado}
            </Typography>

            <Typography variant="h6" component="h2">
              <Box mt={2}>
                <img src={getImagePath()} alt="Carro Selecionado" style={{ width: '300px' }} />
              </Box>
              {marca}, {modelo}, {motor}
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

            {/* {diagnostico.pergunta.includes("(Sim/Não)") && !loading && (
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
            )} */}

            {diagnostico.pergunta && !loading && (
              <Box sx={{ mt: 3 }}>

                {/* (Sim/Não) */}
                {diagnostico.pergunta.includes("(Sim/Não)") && (
                  <>
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
                  </>
                )}

                {/* (Baixo/Alto/Normal) */}
                {diagnostico.pergunta.includes("(Baixo/Alto/Normal)") && (
                  <>
                    <Button
                      onClick={() => handleAnswer("Baixo")}
                      variant="contained"
                      color="primary"
                      sx={{ mr: 2 }}>
                      Baixo
                    </Button>
                    <Button
                      onClick={() => handleAnswer("Normal")}
                      variant="contained"
                      color="primary"
                      sx={{ mr: 2 }}>
                      Normal
                    </Button>
                    <Button
                      onClick={() => handleAnswer("Alto")}
                      variant="contained"
                      color="primary">
                      Alto
                    </Button>
                  </>
                )}

                {/* (Minimo/Maximo/Normal) */}
                {diagnostico.pergunta.includes("(Minimo/Maximo/Normal)") && (
                  <>
                    <Button
                      onClick={() => handleAnswer("Minimo")}
                      variant="contained"
                      color="primary"
                      sx={{ mr: 2 }}>
                      Minimo
                    </Button>
                    <Button
                      onClick={() => handleAnswer("Normal")}
                      variant="contained"
                      color="primary"
                      sx={{ mr: 2 }}>
                      Normal
                    </Button>
                    <Button
                      onClick={() => handleAnswer("Maximo")}
                      variant="contained"
                      color="primary">
                      Maximo
                    </Button>
                  </>
                )}
                
                {/* "(Number.Number / Number.Number) Litros" */}
                {diagnostico.pergunta.match(/\s+\((\d+\.\d+) \/ (\d+\.\d+)\)Litros/) && (
                  <>
                    {(() => {
                      const match = diagnostico.pergunta.match(/\s+\((\d+\.\d+) \/ (\d+\.\d+)\)Litros/);
                      const minValue = match ? parseFloat(match[1]) : 0;
                      const maxValue = match ? parseFloat(match[2]) : 0;

                      return (
                        <>
                          {/* <FloatInput min={minValue} max={maxValue} onValueChange={setNivelOleo} /> */}
                          <TextField 
                            type="number" 
                            step="0.1" 
                            min={minValue} 
                            max={maxValue} 
                            value={nivelOleo !== null ? nivelOleo : minValue}
                            //onValueChange={setNivelOleo(parseFloat(this))} 
                            onChange={(e) => setNivelOleo(parseFloat(e.target.value))}
                            style={{
                              width:"100px",
                            }}
                            InputProps={{
                              inputProps: {
                                max: maxValue, 
                                min: minValue
                              }
                            }}
                          />
                          
                          <Button
                            onClick={() => handleAnswer(nivelOleo)}
                            variant="contained"
                            color="primary"
                            sx={{ ml: 2, mt: 2 }}
                          >
                            Enviar resposta
                          </Button>
                        </>
                      );
                    })()}
                  </>
                )}

                {/* Seleção de uma lista de componentes */}
                {diagnostico.pergunta.match(/Verifique os seguintes componentes\:/) && (
                  <>
                    {diagnostico.pergunta.match(/\d+/g).map((num, index) => (
                      <Button
                        key={index}
                        onClick={() => handleAnswer(num)}
                        variant="contained"
                        color="primary"
                        sx={{ mr: 2 }}>
                        {num}
                      </Button>
                      // 1. Bomba de água
                    //   <Button
                    //   onClick={() => handleAnswer("0")}
                    //   variant="contained"
                    //   color="primary">
                    //   0-Nenhum
                    // </Button>
                    ))}
                  </>
                )}
              </Box>
            )}

          </CardContent>

          {/* DEBUG */}
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

export default DiagnosticoCarro;

// Opções de resposta existentes:
// (Sim/Não)
// (Baixo/Alto/Normal)
// (Minimo/Maximo/Normal)
// "Verifique os seguintes componentes:\n1. Bomba de água\n2. Alternador\n3. Compressor de ar condicionado\n4. Tensores da correia\nDigite o número correspondente ao componente com defeito, ou 0 se nenhum:"
// (Entre " + oleoMotor.getValorMinimo() + " e " + oleoMotor.getValorMaximo() + " " + oleoMotor.getUnidade() + ")
// Qual é o nível atual de óleo? (Entre 0.0 e 8.0 Litros)

