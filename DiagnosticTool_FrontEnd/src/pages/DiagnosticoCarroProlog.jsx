import { Alert, Box, Button, CircularProgress, Typography } from "@mui/material";
import axios from 'axios';
import { InputNumber } from 'primereact/inputnumber';
import { useCallback, useEffect, useState } from 'react';
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

  const fetchPergunta = useCallback(async () => {
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
  }, [API_URL, navigate]);

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
  }, [fetchPergunta]);

  return { diagnostico, loading, error, handleAnswer };
};

const DiagnosticoCarroProlog = () => {

  const { diagnostico, loading, error, handleAnswer } = useDiagnostico();
  const [numericAnswer, setNumericAnswer] = useState();

  if (!diagnostico) return null;

  const isNumericInput = 
    Array.isArray(diagnostico.respostas) && 
    diagnostico.respostas.length === 2 &&
    typeof diagnostico.respostas[0] === 'number' && 
    typeof diagnostico.respostas[1] === 'number';

  const handleNumericInputChange = (e) => {
    setNumericAnswer(e.target.value);
  };

  const submitNumericAnswer = () => {
    const answer = parseFloat(numericAnswer);
    if (answer >= diagnostico.respostas[0] && answer <= diagnostico.respostas[1]) {
      handleAnswer(answer);
    } else {
      alert(`Please enter a value between ${diagnostico.respostas[0]} and ${diagnostico.respostas[1]}`);
    }
  };

  return (
    <CardWrapper titulo={`Questionário Diagnóstico`}>
      <TituloLinha title="Pergunta" lineColor="white" icon="HelpTwoTone" position="13px" />
      
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

      {/* {diagnostico.respostas && !loading && (
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
      )} */}

      {isNumericInput ? (
        <Box sx={{ display: 'flex', alignItems: 'center', justifyContent: 'center', mt: 2 }}>
          <InputNumber
            value={numericAnswer}
            onValueChange={handleNumericInputChange}
            //onChange={handleNumericInputChange}
            min={diagnostico.respostas[0]}
            max={diagnostico.respostas[1]}
            step={0.1}
            showButtons
            buttonLayout="vertical"
            style={{ height:'5rem', width: '5rem' }}
            //decrementButtonClassName="p-button-secondary" incrementButtonClassName="p-button-secondary"
            //incrementButtonIcon="pi pi-plus" decrementButtonIcon="pi pi-minus"
            //suffix=" unidade"
          />

        <Button onClick={submitNumericAnswer} variant="contained" color="primary" sx={{ ml: 2 }}>Enviar resposta</Button>
      </Box>
      ) : (
        <Box sx={{ mt: 3, display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
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
