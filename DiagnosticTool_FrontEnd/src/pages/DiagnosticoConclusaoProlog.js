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
import { getApiUrl } from '../config/apiConfig';
import { generatePDF } from '../utils/pdfConclusaoProlog';

const DiagnosticoConclusaoProlog = () => {
  const [diagnostico, setDiagnostico] = useState([]);
  const [solucao, setSolucao] = useState([]);
  const [como, setComo] = useState([]);
  const [responseText, setResponseText] = useState(null);
  const [activeButtonIndex, setActiveButtonIndex] = useState(null);
  const [responseTextVisible, setResponseTextVisible] = useState(false);
  const [falhaResponseTextVisible, setFalhaResponseTextVisible] = useState(false);
  const [falhas, setFalhas] = useState([]);
  const [falha, setFalha] = useState('');
  const [falhaDetalhes, setFalhaDetalhes] = useState(null);
  const [parsedFalhaDetalhes, setParsedFalhaDetalhes] = useState([]);
  // let arvorePerguntas = {
  //   label: '',
  //   expanded: 'true',
  //   data: {
  //     pecaCarro:
      
  //   }
  //   children: []
  // }

  useEffect(() => {
    fetchDiagnostico();
    fetchComo();
    fetchFalhas();
  }, []);

  const fetchDiagnostico = async () => {
    try {
      const response = await fetch(`${getApiUrl()}/diagnostico`);
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
      const response = await fetch(`${getApiUrl()}/como`);
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
      const response = await fetch(`${getApiUrl()}/diagnosticoPossiveis`);
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
      const response = await fetch(`${getApiUrl()}/porque`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json; charset=utf-8' },
        body: JSON.stringify(body),
      });

      if (response.ok) {
        const data = await response.json();
        const resultado = [];
        // const formattedResponse = data.pergunta_anterior
        //   ? `${data.explicacao}.\nNa pergunta "${data.pergunta_anterior}" a resposta foi "${data.resposta_anterior}".`
        //   : `${data.explicacao}.`;

        const porque = data.explicacao;
        const motivo = data.pergunta_anterior ? `Na pergunta "${data.pergunta_anterior}" a resposta foi "${data.resposta_anterior}".`
          : null;
        resultado.push({ porque, motivo });

        setResponseText([ porque, motivo ]);
        //setResponseText(formattedResponse);
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

      const body = { facto: `diagnostico(Veiculo, '${selectedFalha}')` };
      try {
        const response = await fetch(`${getApiUrl()}/porqueNao`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json; charset=utf-8' },
          body: JSON.stringify(body),
        });

        if (response.ok) {
          const data = await response.json();

          if (data){
            setFalhaDetalhes(data);

            if(data.explanation){
              setParsedFalhaDetalhes(parseExplanation(data.explanation));
            }

            setFalhaResponseTextVisible(false);
            setTimeout(() => setFalhaResponseTextVisible(true), 100);
          }
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

  const parseExplanation = (value) => {
    const lines = value.split('\n');
    const results = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      
      if (line.startsWith("Porque")) {
        const porque = line;
        const nextLine = lines[i + 1] ? lines[i + 1].trim() : '';
        
        if (nextLine) {
          const motivo = nextLine;
          results.push({ porque, motivo });
          i++;
        }
      }
    }

    return results;
  };

  return (
    <CardWrapper titulo={`Diagnóstico Concluído`}>
      <Box sx={{ position: 'absolute', top: 16, right: 80 }}>
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
              responseText, // porque
              activeButtonIndex,
              responseTextVisible,
              falha,
              parsedFalhaDetalhes, // falhaDetalhes, // porque nao
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
        <AccordionSummary expandIcon={<ExpandMoreIcon />}>
          <SvgIcon component={LightbulbCircleIcon} sx={{ paddingLeft: '0px', marginRight: '5px', fontSize: '2rem', color: 'inherit' }} />
          <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left', color: 'inherit' }}>
            Diagnóstico Geral
          </Typography>
          <Typography variant="h6" sx={{ color: 'inherit' }}>{diagnostico}</Typography>
        </AccordionSummary>
        <AccordionDetails>
          <List>
            <ListItem>
              <Typography variant="h6" gutterBottom sx={{ color: 'lime' }}>
                Diagnóstico: {diagnostico}
              </Typography>
            </ListItem>
            <ListItem>
              <Typography variant="h6" gutterBottom sx={{ color: 'lime' }}>
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
                                        //color: 'lime',
                                        whiteSpace: 'pre-line'
                                        }}
                                      >
                                      <strong>Porque: {responseText[0]}</strong><br/>
                                      {responseText[1] && (<em>{responseText[1]}</em>)}
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
                        {parsedFalhaDetalhes.map((item, index) => (
                          <Box sx={{ mt: 2, pl: 2, borderLeft: '3px solid lime', whiteSpace:'pre-line' }}>
                              <Typography variant="body2" key={index}>
                                  <strong>{item.porque}</strong><br />
                                  {item.motivo.length>0 && (<em>{item.motivo}</em>)}
                              </Typography>
                            </Box>
                          ))}
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
