import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import Help from '@mui/icons-material/Help';
import InfoIcon from '@mui/icons-material/Info';
import LightbulbCircleIcon from '@mui/icons-material/LightbulbCircle';
import PictureAsPdfIcon from '@mui/icons-material/PictureAsPdf';
import {
  Accordion, AccordionDetails, AccordionSummary,
  Autocomplete,
  Box,
  Button,
  Collapse,
  FormControl,
  Grid,
  IconButton,
  List, ListItem,
  SvgIcon,
  TextField,
  Typography
} from '@mui/material';
import React, { useEffect, useState } from 'react';
import CardWrapper from "../components/CardWrapper";
import { generatePDF } from '../utils/pdfConclusaoProlog';

const DiagnosticoConclusaoProlog = () => {
  const [diagnostico, setDiagnostico] = useState([]);
  const [solucao, setSolucao] = useState([]);
  const [como, setComo] = useState([]);
  const [responseText, setResponseText] = useState('');
  const [activeButtonIndex, setActiveButtonIndex] = useState(null);
  const [responseTextVisible, setResponseTextVisible] = useState(false);
  const [falhaResponseTextVisible, setFalhaResponseTextVisible] = useState(false);
  const [falhas, setFalhas] = useState([]);
  const [falha, setFalha] = useState('');
  const [falhaDetalhes, setFalhaDetalhes] = useState(null);

  useEffect(() => {
    fetchDiagnostico();
    fetchComo();
    fetchFalhas();
  }, []);

  const fetchDiagnostico = async () => {
    try {
      const response = await fetch('http://localhost:4040/diagnostico');
      if (response.ok) {
        const data = await response.json();
        setDiagnostico(data.diagnostico || []);
        setSolucao(data.solucao || []);
      } else {
        console.error('Erro carregando diagnostico:', response.statusText);
      }
    } catch (error) {
      console.error('Fetch error:', error);
    }
  };

  const fetchComo = async () => {
    try {
      const response = await fetch('http://localhost:4040/como');
      if (response.ok) {
        const data = await response.json();
        setComo(data || []);
      } else {
        console.error('Erro carregando dados do "como":', response.statusText);
      }
    } catch (error) {
      console.error('Fetch error:', error);
    }
  };

  const fetchFalhas = async () => {
    try {
      const response = await fetch('http://localhost:4040/diagnosticoPossiveis');
      if (response.ok) {

        const data = await response.json();

        // Filtra o diagnóstico encontrado.
        //Alterei para filtragem acontecer no componente, para ter todos os dados dos diagnósticos possíveis
        //const filteredFalhas = data.diagnosticos?.filter(falha => falha != diagnostico) || [];

        const filteredFalhas = data.diagnosticos || [];
        

        const uniqueFalhas = Array.from(new Set(filteredFalhas));
        setFalhas(uniqueFalhas);

      } else {
        console.error('Erro carregando dados do "diagnosticosPossiveis":', response.statusText);
      }
    } catch (error) {
      console.error('Fetch error:', error);
    }
  };


  const handlePorque = async (fact, index) => {
    const body = { facto: fact };
    try {
      const response = await fetch('http://localhost:4040/porque', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body),
      });

      if (response.ok) {
        const data = await response.json();
        const formattedResponse = data.pergunta_anterior
          ? `${data.explicacao}.\nNa pergunta "${data.pergunta_anterior}" a resposta foi "${data.resposta_anterior}".`
          : `${data.explicacao}.`;

        setResponseText(formattedResponse);
        setActiveButtonIndex(index);

        setResponseTextVisible(false);
        setTimeout(() => setResponseTextVisible(true), 200);

      } else {
        console.error('Erro buscando diagnostico:', response.statusText);
      }
    } catch (error) {
      console.error('Fetch error:', error);
    }
  };

  const handleFalhasChange = async (event, value) => {
    //const selectedFalha = event.target.value; // Usar se for componente Select
    const selectedFalha = value;

    if (selectedFalha){
      setFalha(selectedFalha);

    //   {
    //     "explanation": "Porque pela regra 55:\n
    //      A premissa filtro_combustivel_entupido(_6452,sim) é falsa\n
    //           Porque pela regra 46:\n
    //                A premissa falta_combustivel_ou_bomba_defeito(_6604,nao) é falsa\n
    //                     Porque pela regra 34:\n
    //                          A premissa motor_sobreaquece(_6756,nao) é falsa\n
    //                               Porque pela regra 23:\n
    //                                    A premissa vai_abaixo(_6908,sim) é falsa\n
    //                                         Parou no nivel9"
    // }

      const body = { facto: `diagnostico(Veiculo, '${selectedFalha}')` };
      try {
        const response = await fetch('http://localhost:4040/porqueNao', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(body),
        });

        if (response.ok) {
          const data = await response.json();
          setFalhaDetalhes(data);

          setFalhaResponseTextVisible(false);
          setTimeout(() => setFalhaResponseTextVisible(true), 100);
        } else {
          console.error('Erro buscando detalhes porqueNao:', response.statusText);
        }
      } catch (error) {
        console.error('Fetch error:', error);
      }
  } else {
    setFalhaResponseTextVisible(false);
  }
  };

  return (
    <CardWrapper titulo={`Diagnóstico Concluído`}>
      <Box sx={{ position: 'absolute', top: 16, right: 70 }}>
        <IconButton
          variant="contained"
          onClick={() =>
            generatePDF(
              null, 
              diagnostico, 
              solucao, 
              null, 
              null, 
              como, 
              null, 
              null, 
              responseText, 
              activeButtonIndex, 
              responseTextVisible, 
              falha,
              falhaDetalhes, 
              falhaResponseTextVisible ? falhaDetalhes : null
            )
            //generatePDF(null, diagnostico, solucao, null, null, como, null, null)
          }
          sx={{ color: 'red', backgroundColor: 'white', marginLeft:'10px',
            '&:hover': { backgroundColor: 'rgba(255, 0, 0, 0.1)', },
          }}
        >
          <PictureAsPdfIcon />
        </IconButton>
      </Box>
      <Accordion defaultExpanded variant="outlined">
        <AccordionSummary expandIcon={<ExpandMoreIcon />} sx={{ color: 'lime' }}>
          <SvgIcon component={LightbulbCircleIcon} sx={{ paddingLeft: '0px', marginRight: '5px', fontSize: '2rem', color: 'inherit' }} />
          <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left', color: 'inherit' }}>
            Diagnóstico Geral
          </Typography>
          <Typography variant="h6" sx={{ color: 'inherit' }}>{diagnostico}</Typography>
        </AccordionSummary>
        <AccordionDetails>
          <List>
            <ListItem>
              <Typography variant="h6" gutterBottom>
                Diagnóstico: {diagnostico}
              </Typography>
            </ListItem>
            <ListItem>
              <Typography variant="h6" gutterBottom>
                Solução: {solucao}
              </Typography>
            </ListItem>
          </List>
        </AccordionDetails>
      </Accordion>

      <Accordion defaultExpanded variant="outlined">
        <AccordionSummary expandIcon={<ExpandMoreIcon />}>
          <SvgIcon component={InfoIcon} sx={{ paddingLeft: '0px', marginRight: '5px', fontSize: '2rem', color: 'inherit' }} />
          <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left' }}>
            Explicações
          </Typography>
          <Typography variant="h6" sx={{ color: 'text.secondary' }}><i>Entenda o diagnóstico</i></Typography>
        </AccordionSummary>
        <AccordionDetails>

          <Grid container spacing={2} alignItems="center">
              <Grid item xs={12}>
                <Accordion variant="elevation" defaultExpanded>
                  <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                    <Typography variant="h6"><em>Como</em> chegou-se ao diagnóstico? <em>Porque</em> estas evidências?</Typography>
                  </AccordionSummary>
                  <AccordionDetails>
                    <List>
                      <ListItem>O diagnóstico foi obtido com os seguintes factos. Veja o porque de cada evidência.</ListItem>
                      {como.length > 0 ? (
                        como.map((evidencia, index) => (
                          evidencia.pergunta ? (
                            <ListItem key={index} sx={{ display: 'flex', justifyContent: 'space-between', width: '100%' }}>
                              <Grid container spacing={2} alignItems="center">
                                <Grid item xs>
                                  <Typography variant="body1">
                                    Pergunta: {evidencia.pergunta}<br/>
                                    Resposta: {evidencia.resposta}
                                  </Typography>
                                  {/* <Collapse in={responseTextVisible} timeout={500}> */}
                                  {activeButtonIndex === index && responseText && (
                                    <Box
                                    sx={{
                                      mt: 2,
                                      pl: 2,
                                      opacity: responseTextVisible ? 1 : 0,
                                      visibility: responseTextVisible ? 'visible' : 'hidden',
                                      //display: responseTextVisible ? 'block' : 'none',
                                      transition: 'opacity 0.8s ease-in-out, visibility 0.8s ease-in-out',
                                      borderLeft: '3px solid lime',
                                    }}
                                  >
                                      <Typography variant="body2" sx={{
                                        mt: 1,
                                        color: 'lime',
                                        whiteSpace: 'pre-line'
                                        }}
                                      >
                                      {responseText}
                                    </Typography>
                                  </Box>
                                  )}
                                  {/* </Collapse> */}
                                </Grid>
                                <Grid item>
                                  <Button
                                    variant="contained"
                                    endIcon={<Help />}
                                    onClick={() => handlePorque(evidencia.fato, index)}
                                  >
                                    Porque
                                  </Button>
                                </Grid>
                              </Grid>
                            </ListItem>
                          ) : null
                        ))
                      ) : (
                        <ListItem>Nenhuma evidência encontrada.</ListItem>
                      )}
                    </List>
                  </AccordionDetails>
                </Accordion>
              </Grid>
              
              <Grid item xs={12}>
              <Accordion variant="elevation" defaultExpanded>
                  <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                    <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left' }}>
                        <em>Porque Não</em> Outro Diagnóstico?
                    </Typography>
                  </AccordionSummary>
                  <AccordionDetails>
                    {falhas.length > 0 && (
                      <FormControl fullWidth>
                        <Autocomplete
                          options={falhas.filter((f) => f !== diagnostico)} // Filtra o diagnostico final
                          value={falha}
                          onChange={handleFalhasChange}
                          renderInput={(params) => (
                            <TextField {...params} label="Todos outros diagnósticos possíveis" variant="outlined" />
                          )}
                        />
                      </FormControl>
                    )}
                    
                    <Collapse in={falhaResponseTextVisible} timeout={500}>
                    {falhaDetalhes && falhaResponseTextVisible && (
                      <Box sx={{ mt: 2 }}>
                        <Box sx={{ mt: 2, pl: 2, borderLeft: '3px solid lime', whiteSpace:'pre-line' }}>
                            <Typography variant="body2">
                              {falhaDetalhes.explanation}
                            </Typography>
                          </Box>
                        {/* {Array.isArray(falhaDetalhes) ? (
                          [...falhaDetalhes]
                            .sort((a, b) => a.explicacao.localeCompare(b.explicacao))
                            .map((detalhe, index) => (
                              <Box key={index} sx={{ mt: 2, pl: 2, borderLeft: '3px solid lime' }}>
                                <Typography variant="body2">
                                  {detalhe.explicacao} {detalhe.regra_id}:
                                </Typography>
                                <List>
                                  {detalhe.detalhes.map((det, idx) => (
                                    <ListItem key={idx} sx={{ pl: 2 }}>
                                      <Typography variant="body1" sx={{ color: 'lime' }}>
                                        {det.explicacao} - {det.premissa} {det.condicao}
                                      </Typography>
                                    </ListItem>
                                  ))}
                                </List>
                              </Box>
                            ))
                        ) : 
                        (
                          <Box sx={{ mt: 2, pl: 2, borderLeft: '3px solid red' }}>
                            <Typography variant="body2">
                              {falhaDetalhes}
                            </Typography>
                            <Typography variant="body2">
                              Selecione outro diagnóstico
                            </Typography>
                          </Box>
                        )} */}
                      </Box>
                    )}
                    </Collapse>
                  </AccordionDetails>
                </Accordion>
              </Grid>
          </Grid>
        </AccordionDetails>
      </Accordion>
    </CardWrapper>
  );
};

export default DiagnosticoConclusaoProlog;
