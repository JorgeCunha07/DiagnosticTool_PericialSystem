import { Alert, Box, Button, CircularProgress, Typography } from "@mui/material";
import axios from 'axios';
import { InputNumber } from 'primereact/inputnumber';
import { useState } from 'react';
import { useLocation, useNavigate } from 'react-router-dom';
import CardWrapper from "../components/CardWrapper";
import TituloLinha from "../components/TituloLinha";
import { getApiUrl } from '../config/apiConfig';

const tamanho_img = '240px';

const useDiagnostico = (initialData) => {
  const [diagnostico, setDiagnostico] = useState(initialData);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const navigate = useNavigate();

  const handleAnswer = async (answer) => {
    setLoading(true);

    const requestBody = {
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
      const response = await axios.post(`${getApiUrl()}/diagnostico/responder`, requestBody);

      if (response.data.estado === 'finalizado') {
        navigate('/conclusao', { state: { responseData: response.data } });
      } else if (!response.data || !response.data.carroSelecionado.marca || !response.data.carroSelecionado.hasOwnProperty("marca")) {
        navigate('/error', { state: { responseData: response.data || 'Não recebeu dados válidos.' } });
      } else {
        setDiagnostico(response.data);
      }

    } catch (err) {
      setError('Falha ao enviar resposta.');
    } finally {
      setLoading(false);
    }
  };

  return { diagnostico, loading, error, handleAnswer };
};

const DiagnosticoCarro = () => {
  const location = useLocation();

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
    <CardWrapper titulo={`Questionário Diagnóstico`}>
      
      <Box sx={{ mt: 3 , display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
        <Typography variant="h6" component="h2">
          <Box mt={2}>
            <img src={getImagePath()} alt="Carro Selecionado" style={{ width:tamanho_img, margin: 0, padding: 0 }} />
          </Box>
          <Box sx={{ mt: 3 , marginTop: 0, marginBottom: "20px", display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
            {marca} {modelo} {motor}</Box>
        </Typography>
      </Box>

      <TituloLinha title="Pergunta" lineColor="white" icon="HelpTwoTone" position="13px" />
      <Box sx={{ mt: 3 , display: 'flex', justifyContent: 'center', alignItems: 'center', whiteSpace:'pre-line', paddingBottom:'70px' }}>
        <Typography variant="h5" component="h2" sx={{ mt: 2 }}>
          {diagnostico.pergunta.split('?')[0] + '?'}
        </Typography>
      </Box>

      <Box sx={{ height: '0.5px', width: "100%", background: 'white', marginBottom: '30px'}} />

      <Box sx={{ mt: 3 , display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
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
                      <Box sx={{ display: 'flex', alignItems: 'center', justifyContent: 'center', mt: 2 }}>
                        <InputNumber
                          value={nivelOleo}
                          onValueChange={(e) => setNivelOleo(e.value)}
                          min={minValue}
                          max={maxValue}
                          step={0.1}
                          showButtons
                          buttonLayout="vertical"
                          style={{ height:'5rem', width: '5rem' }}
                          //decrementButtonClassName="p-button-secondary" incrementButtonClassName="p-button-secondary"
                          //incrementButtonIcon="pi pi-plus" decrementButtonIcon="pi pi-minus"
                          //suffix=" unidade"
                        />

                        <Button onClick={() => handleAnswer(nivelOleo)} variant="contained" color="primary" sx={{ ml: 2 }}>Enviar resposta</Button>
                      </Box>
                      {/* <TextField 
                        type="number" 
                        step="0.1" 
                        min={minValue} 
                        max={maxValue} 
                        value={nivelOleo !== null ? nivelOleo : minValue}
                        onChange={(e) => setNivelOleo(parseFloat(e.target.value))}
                        style={{ width:"100px" }}
                        InputProps={{ inputProps: { max: maxValue, min: minValue } }}
                      /> */}
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
      </Box>
    </CardWrapper>
  );
};

export default DiagnosticoCarro;
