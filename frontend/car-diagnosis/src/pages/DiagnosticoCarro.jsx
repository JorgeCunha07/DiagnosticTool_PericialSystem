import HomeIcon from '@mui/icons-material/Home';
import { Alert, Box, Button, Card, CardContent, CircularProgress, Container, IconButton, TextField, Typography } from "@mui/material";
import axios from 'axios';
import { useState } from 'react';
import { useLocation, useNavigate } from 'react-router-dom';
import { getApiUrl, getSistemaSelecionado } from '../config/apiConfig';

const sistemaSelecionado = getSistemaSelecionado();

const useDiagnostico = (initialData) => {
  const [diagnostico, setDiagnostico] = useState(initialData);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const navigate = useNavigate();
  const API_URL = getApiUrl();

  console.log(">>>>>>> DiagnosticoCarro: diagnostico:" + diagnostico);

  const handleAnswer = async (answer) => {
    setLoading(true);

    let requestBody = {};
    let endpoint = '';
    let prolog_response;

    if (sistemaSelecionado === 'Drools') {

      endpoint = '/diagnostico/responder';

      requestBody = {
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
        triggeredRules: diagnostico.triggeredRules,
      };

      try {
        const response = await axios.post(`${API_URL}${endpoint}`, requestBody);
  
        if (response.data.estado === 'finalizado') {
          navigate('/conclusao', { state: { responseData: response.data } });
        } else if (
          !response.data ||
          !response.data.carroSelecionado.marca ||
          !response.data.carroSelecionado.hasOwnProperty("marca")
        ) {
          navigate('/error', { state: { responseData: response.data || 'Não recebeu dados válidos.' } });
        } else {
          setDiagnostico(response.data);
        }
      } catch (err) {
        setError('Falha ao enviar resposta.');
      } finally {
        setLoading(false);
      }

    } else if (sistemaSelecionado === 'PROLOG') {

      // PARA A PERGUNTA
      endpoint = '/pergunta';  
      // localhost:4040/pergunta | GET
      // sem body
      // Resposta:
      // {
      //   "estado": "ongoing",
      //   "pergunta": "O veiculo Opel Astra J 1.6 CDTi tem algum problema?",
      //   "respostas": [
      //       "sim",
      //       "nao",
      //       "nao_sei"
      //   ]
      // }

      // PARA O RESPONDER
      //endpoint = '/responder';  
      // localhost:4040/responder | POST
      // no body do post: {"resposta": "sim"}
      // Resposta:
      // {
      //   "message": "Respondido",
      //   "status": "OK"
      // }Foi concluido o facto proximo_teste numero 2 -> proximo_teste(2,liga)

      requestBody = {
        texto: answer,
        estado: diagnostico?.estado || 'N/A',
        pergunta: diagnostico?.pergunta || 'N/A',
        carroSelecionado: diagnostico?.carroSelecionado || 'N/A',
        marcaSelecionada: 'N/A',
        modeloSelecionado: 'N/A',
        motorSelecionado: 'N/A',
        diagnostico: 'N/A',
        solucao: 'N/A',
        explicacaoGeral: 'N/A',
        explicacaoGeralNao: 'N/A',
        como: 'N/A',
        evidencias: 'N/A',
        triggeredRules: 'N/A',
      };

      prolog_response = await axios.get(`${API_URL}${endpoint}`);

      const estado = prolog_response.data.estado;
      const pergunta = prolog_response.data.pergunta;
      const respostas = prolog_response.data.respostas;
      
      console.log(">>>> DiagnosticoCarro: estado: " + estado);
      console.log(">>>> DiagnosticoCarro: pergunta: " + pergunta);
      console.log(">>>> DiagnosticoCarro: respostas: " + respostas);

      try {
        const response = await axios.post(`${API_URL}${endpoint}`, requestBody);
  
        if (response.data.estado === 'finalizado') {
          navigate('/conclusao', { state: { responseData: response.data } });
        } else if (
          !response.data
        ) {
          navigate('/error', { state: { responseData: response.data || 'Não recebeu dados válidos.' } });
        } else {
          setDiagnostico(response.data);
        }
      } catch (err) {
        setError('Falha ao enviar resposta.');
      } finally {
        setLoading(false);
      }
    }

    // try {
    //   const response = await axios.post(`${API_URL}${endpoint}`, requestBody);

    //   if (response.data.estado === 'finalizado') {
    //     navigate('/conclusao', { state: { responseData: response.data } });
    //   } else if (
    //     !response.data ||
    //     !response.data.carroSelecionado.marca ||
    //     !response.data.carroSelecionado.hasOwnProperty("marca")
    //   ) {
    //     navigate('/error', { state: { responseData: response.data || 'Não recebeu dados válidos.' } });
    //   } else {
    //     setDiagnostico(response.data);
    //   }
    // } catch (err) {
    //   setError('Falha ao enviar resposta.');
    // } finally {
    //   setLoading(false);
    // }
  };

  return { diagnostico, loading, error, handleAnswer };
};

// const useDiagnostico = (initialData) => {
//   const [diagnostico, setDiagnostico] = useState(initialData);
//   const [loading, setLoading] = useState(false);
//   const [error, setError] = useState(null);
//   const navigate = useNavigate();

//   const handleAnswer = async (answer) => {
//     setLoading(true);

//     const requestBody = {
//       texto: answer,
//       estado: diagnostico.estado,
//       pergunta: diagnostico.pergunta,
//       carroSelecionado: diagnostico.carroSelecionado,
//       marcaSelecionada: diagnostico.carroSelecionado.marca.nome,
//       modeloSelecionado: diagnostico.carroSelecionado.modelo.nome,
//       motorSelecionado: diagnostico.carroSelecionado.motor.nome,
//       diagnostico: diagnostico.diagnostico,
//       solucao: diagnostico.solucao,
//       explicacaoGeral: diagnostico.explicacaoGeral,
//       explicacaoGeralNao: diagnostico.explicacaoGeralNao,
//       como: diagnostico.como,
//       evidencias: diagnostico.evidencias,
//       triggeredRules: diagnostico.triggeredRules,
//     };

//     try {
//       const response = await axios.post(`${getApiUrl()}/diagnostico/responder`, requestBody);

//       if (response.data.estado === 'finalizado') {
//         navigate('/conclusao', { state: { responseData: response.data } });
//       } else if (!response.data || !response.data.carroSelecionado.marca || !response.data.carroSelecionado.hasOwnProperty("marca")) {
//         navigate('/error', { state: { responseData: response.data || 'Não recebeu dados válidos.' } });
//       } else {
//         setDiagnostico(response.data);
//       }

//     } catch (err) {
//       setError('Falha ao enviar resposta.');
//     } finally {
//       setLoading(false);
//     }
//   };

//   return { diagnostico, loading, error, handleAnswer };
// };

const DiagnosticoCarro = () => {
  const location = useLocation();
  const navigate = useNavigate();

  const [nivelOleo, setNivelOleo] = useState(0.0);
  
  const { diagnosticoData } = location.state;
  const { diagnostico, loading, error, handleAnswer } = useDiagnostico(diagnosticoData);

  if (!diagnosticoData) return null;

  const marca = diagnostico?.carroSelecionado.marca?.nome || 'N/A';
  const modelo = diagnostico?.carroSelecionado.modelo?.nome || 'N/A';
  const motor = diagnostico?.carroSelecionado.motor?.nome || 'N/A';

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
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="100vh">
        <Card sx={{ position: 'relative', padding: 2 }}>
          <Box sx={{ position: 'absolute', top: 16, right: 16}}>
            <IconButton
              variant="contained"
              onClick={()=>navigate('/')}
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

            {diagnostico.pergunta && !loading && (
              <Box sx={{ mt: 3 }}>
                {diagnostico.pergunta.includes("(Sim/Não)") && (
                  <>
                    <Button onClick={() => handleAnswer("Sim")} variant="contained" color="primary" sx={{ mr: 2 }}>Sim</Button>
                    <Button onClick={() => handleAnswer("Não")} variant="contained" color="secondary">Não</Button>
                  </>
                )}

                {diagnostico.pergunta.includes("(Baixo/Alto/Normal)") && (
                  <>
                    <Button onClick={() => handleAnswer("Baixo")} variant="contained" color="primary" sx={{ mr: 2 }}>Baixo</Button>
                    <Button onClick={() => handleAnswer("Normal")} variant="contained" color="primary" sx={{ mr: 2 }}>Normal</Button>
                    <Button onClick={() => handleAnswer("Alto")} variant="contained" color="primary">Alto</Button>
                  </>
                )}

                {diagnostico.pergunta.includes("(Minimo/Maximo/Normal)") && (
                  <>
                    <Button onClick={() => handleAnswer("Minimo")} variant="contained" color="primary" sx={{ mr: 2 }}>Minimo</Button>
                    <Button onClick={() => handleAnswer("Normal")} variant="contained" color="primary" sx={{ mr: 2 }}>Normal</Button>
                    <Button onClick={() => handleAnswer("Maximo")} variant="contained" color="primary">Maximo</Button>
                  </>
                )}
                
                {diagnostico.pergunta.match(/\s+\((\d+\.\d+) \/ (\d+\.\d+)\)Litros/) && (
                  <>
                    {(() => {
                      const match = diagnostico.pergunta.match(/\s+\((\d+\.\d+) \/ (\d+\.\d+)\)Litros/);
                      const minValue = match ? parseFloat(match[1]) : 0;
                      const maxValue = match ? parseFloat(match[2]) : 0;

                      return (
                        <>
                          <TextField 
                            type="number" 
                            step="0.1" 
                            min={minValue} 
                            max={maxValue} 
                            value={nivelOleo !== null ? nivelOleo : minValue}
                            onChange={(e) => setNivelOleo(parseFloat(e.target.value))}
                            style={{ width:"100px" }}
                            InputProps={{ inputProps: { max: maxValue, min: minValue } }}
                          />
                          
                          <Button onClick={() => handleAnswer(nivelOleo)} variant="contained" color="primary" sx={{ ml: 2, mt: 2 }}>
                            Enviar resposta
                          </Button>
                        </>
                      );
                    })()}
                  </>
                )}
                
                {diagnostico.pergunta.match(/Verifique os seguintes componentes\:/) && (
                  <>
                    {diagnostico.pergunta.match(/\d+/g).map((num, index) => (
                      <Button key={index} onClick={() => handleAnswer(num)} variant="contained" color="primary" sx={{ mr: 2 }}>
                        {num}
                      </Button>
                    ))}
                  </>
                )}
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

export default DiagnosticoCarro;
