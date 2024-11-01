import { Check, Close, Rule } from "@mui/icons-material";
import { Alert, Box, Button, Chip, CircularProgress, IconButton, Step, StepContent, StepLabel, Stepper, Typography } from "@mui/material";
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
  const [history, setHistory] = useState([]);
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
      setHistory((prevHistory) => [
        ...prevHistory,
        { question: diagnostico.pergunta, answer }
      ]);

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

  return { diagnostico, loading, error, handleAnswer, history };
};

const DiagnosticoCarroProlog = () => {

  const { diagnostico, loading, error, handleAnswer, history } = useDiagnostico();
  const [numericAnswer, setNumericAnswer] = useState();
  const [showStepper, setShowStepper] = useState(true);


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

  function formataPerguntaStepper(question) {
    const strSize = 90;
    const str = question.split('?')[0];
    return str.length > strSize ? str.slice(0, strSize-40) + "(...)" : str;
  }

  return (
    <CardWrapper titulo={`Questionário Diagnóstico`}>

      <Box sx={{ position: 'absolute', top: 16, right: 80 }}>
        <IconButton
          variant="contained"
          onClick={() => setShowStepper(!showStepper)}
          //color='primary'
          sx={{ color: 'lime', backgroundColor: '#1b5e20', marginLeft:'10px',
          '&:hover': { backgroundColor: 'rgba(0, 255, 0, 0.1)' },

          }}
        >
          <Rule />
        </IconButton>
      </Box>

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

      {/* Painel da  direita com o follow-up do questionario */}

      {showStepper && (
        <Box
          sx={{
            width: '350px',
            position: 'fixed',
            right: 0,
            top: 0,
            bottom: 0,
            backgroundColor: '#212121',
            padding: 5,
            overflowY: 'auto',
            transition: 'opacity 1000ms ease-in',
          }}
        >
          <Typography variant="h6">Perguntas</Typography>
          <Stepper nonLinear activeStep={history.length} orientation="vertical">
            {history.map((step, index) => (
              <Step key={index} expanded={true}>
                <StepLabel>{`${formataPerguntaStepper(step.question)}?`}</StepLabel>
                <StepContent>
                  <Chip size="small" label={step.answer}
                    icon={step.answer === "sim" ? <Check /> : (step.answer === "nao" ? <Close /> : "")}
                    color={step.answer === "sim" ? "success" : (step.answer === "nao" ? "error" : "warning")}
                  />
                </StepContent>
              </Step>
            ))}
            <Step key="current">
              <StepLabel>{formataPerguntaStepper(diagnostico.pergunta)}?</StepLabel>
              <StepContent>
                <Typography variant="body2"><em>Aguardando resposta</em></Typography>
              </StepContent>
            </Step>
          </Stepper>
        </Box>
      )}

    </CardWrapper>
  );
};

export default DiagnosticoCarroProlog;
