import { Alert, Box, Button, Card, CardContent, CircularProgress, Container, TextField, Typography } from "@mui/material";
import React from 'react';
import { useLocation } from 'react-router-dom';
import useDiagnostico from '../hooks/useDiagnostico';

const DiagnosticoCarro = () => {
  const location = useLocation();
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
        <Card lg={{ minWidth: 600 }}>
          <CardContent>
            <Typography variant="h4" component="h1" gutterBottom>
              Questionário Diagnóstico
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

    {/* (Entre "numero" e "outro numero" "unidade") */}
    {diagnostico.pergunta.match(/\(Entre\s(\d+)\se\s(\d+)\s (\s+)\)/) && (
      <>
        <TextField label="Digite um valor"/>
        {/* <Button
          onClick={() => handleAnswer("Maximo")
          variant="contained"
          color="primary">
        Enviar resposta
        </Button> */}
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

// {diagnostico.pergunta && !loading && (
//   <Box sx={{ mt: 3 }}>
//     {/* (Sim/Não) */}
//     {diagnostico.pergunta.includes("(Sim/Não)") && (
//       <>
//         <Button
//           onClick={() => handleAnswer("Sim")}
//           variant="contained"
//           color="primary"
//           sx={{ mr: 2 }}>
//           Sim
//         </Button>
//         <Button
//           onClick={() => handleAnswer("Não")}
//           variant="contained"
//           color="secondary">
//           Não
//         </Button>
//       </>
//     )}

//     {/* (Baixo/Alto/Normal) */}
//     {diagnostico.pergunta.includes("(Baixo/Alto/Normal)") && (
//       <>
//         <Button
//           onClick={() => handleAnswer("Baixo")}
//           variant="contained"
//           color="primary"
//           sx={{ mr: 2 }}>
//           Baixo
//         </Button>
//         <Button
//           onClick={() => handleAnswer("Normal")}
//           variant="contained"
//           color="primary"
//           sx={{ mr: 2 }}>
//           Normal
//         </Button>
//         <Button
//           onClick={() => handleAnswer("Alto")}
//           variant="contained"
//           color="primary">
//           Alto
//         </Button>
//       </>
//     )}

//     {/* (Minimo/Maximo/Normal) */}
//     {diagnostico.pergunta.includes("(Minimo/Maximo/Normal)") && (
//       <>
//         <Button
//           onClick={() => handleAnswer("Minimo")}
//           variant="contained"
//           color="primary"
//           sx={{ mr: 2 }}>
//           Minimo
//         </Button>
//         <Button
//           onClick={() => handleAnswer("Normal")}
//           variant="contained"
//           color="primary"
//           sx={{ mr: 2 }}>
//           Normal
//         </Button>
//         <Button
//           onClick={() => handleAnswer("Maximo")}
//           variant="contained"
//           color="primary">
//           Maximo
//         </Button>
//       </>
//     )}

//     {/* (Entre "numero" e "outro numero" "unidade") */}
//     {diagnostico.pergunta.match(/\(Entre\s(\d+)\se\s(\d+)\s (\s+)\)/) && (
//       <>
//         {diagnostico.pergunta.match(/\d+/g).map((num, index) => (
//           <Button
//             key={index}
//             onClick={() => handleAnswer(num)}
//             variant="contained"
//             color="primary"
//             sx={{ mr: 2 }}>
//             {num}
//           </Button>
//         ))}
//       </>
//     )}
//   </Box>
// )}
