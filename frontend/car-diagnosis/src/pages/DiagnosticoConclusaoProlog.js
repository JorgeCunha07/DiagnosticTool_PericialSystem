import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import Help from '@mui/icons-material/Help';
import PictureAsPdfIcon from '@mui/icons-material/PictureAsPdf';
import { Accordion, AccordionDetails, AccordionSummary, Box, Button, Grid, IconButton, List, ListItem, Typography } from '@mui/material';
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
        setTimeout(() => setResponseTextVisible(true), 100);

      } else {
        console.error('Erro buscando diagnostico:', response.statusText);
      }
    } catch (error) {
      console.error('Fetch error:', error);
    }
  };

  useEffect(() => {
    fetchDiagnostico();
    fetchComo();
  }, []);

  return (
    <CardWrapper titulo={`Diagnóstico Concluído`}>
      <Box sx={{ position: 'absolute', top: 16, right: 70 }}>
        <IconButton
          variant="contained"
          onClick={() =>
            generatePDF(null, diagnostico, solucao, null, null, como, null, null)
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
          <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left' }}>
            Explicações
          </Typography>
          <Typography variant="h6" sx={{ color: 'text.secondary' }}><i>Entenda o diagnóstico</i></Typography>
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
                        {activeButtonIndex === index && responseText && (
                          <Typography
                            variant="body2"
                            sx={{
                              mt: 1,
                              color: 'lime',
                              whiteSpace: 'pre-line',
                              opacity: responseTextVisible ? 1 : 0,
                              transform: responseTextVisible ? 'translateY(0)' : 'translateY(10px)',
                              transition: 'opacity 0.3s ease, transform 0.3s ease'
                            }}
                          >
                            {responseText}
                          </Typography>
                        )}
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
    </CardWrapper>
  );
};

export default DiagnosticoConclusaoProlog;
