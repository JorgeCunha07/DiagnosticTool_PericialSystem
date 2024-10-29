import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import Help from '@mui/icons-material/Help';
import PictureAsPdfIcon from '@mui/icons-material/PictureAsPdf';
import { Accordion, AccordionDetails, AccordionSummary, Box, Button, FormControl, Grid, IconButton, InputLabel, List, ListItem, MenuItem, Select, Typography } from '@mui/material';
import { default as React, useEffect, useState } from 'react';
import { useLocation } from 'react-router-dom';
import CardWrapper from '../components/CardWrapper';
import { getApiUrl } from '../config/apiConfig';
import { generatePDF } from '../utils/pdfConclusao';

const ConclusionPage = () => {
  const location = useLocation();
  const { responseData } = location.state || {};

  const diagnostico = responseData?.diagnostico || 'N/A';
  const solucao = responseData?.solucao || 'N/A';
  const explicacaoGeral = responseData?.explicacaoGeral || 'N/A';
  const explicacaoGeralNao = responseData?.explicacaoGeralNao || 'N/A';
  const como = responseData?.como || 'N/A';
  const evidencias = responseData?.evidencias || [];
  const triggeredRules = responseData?.triggeredRules || [];
  const carro = responseData?.carroSelecionado || {};

  const [responseText, setResponseText] = useState('');
  const [activeButtonIndex, setActiveButtonIndex] = useState(null);
  const [falhaResponseTextVisible, setFalhaResponseTextVisible] = useState(false);
  const [falhasData, setFalhasData] = useState([]);
  const [falhas, setFalhas] = useState([]);
  const [falha, setFalha] = useState('');
  const [falhaDetalhes, setFalhaDetalhes] = useState(null);
  const [responseTextVisible, setResponseTextVisible] = useState(false);


  useEffect(() => {
  const fetchFalhas = async () => {
    try {
      const response = await fetch('http://localhost:8080/api/diagnostico/caminhosDiagnostico', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ triggeredRules: triggeredRules }),
      });

      if (response.ok) {
        const data = await response.json();

        setFalhasData(data);
        console.log(data);

        setFalhas(Object.keys(data));
        
      } else {
        console.error('Error fetching falhas:', response.statusText);
      }
    } catch (error) {
      console.error('Fetch error:', error);
    }
  };

  fetchFalhas();
}, [triggeredRules]);

  const handleFalhasChange = async (event) => {
    const selectedFalha = event.target.value;
    setFalha(selectedFalha);
  
    if (selectedFalha) {
      const selectedFalhaDetails = falhasData[selectedFalha];
      
      setFalhaDetalhes(selectedFalhaDetails);
  
      setFalhaResponseTextVisible(false);
      setTimeout(() => setFalhaResponseTextVisible(true), 100);
    } else {
      console.error('Erro buscando detalhes para:', selectedFalha);
    }
  };
  

  const handlePorqueClick = async (fact, index) => {
    const perguntaAtual = fact;

    const body = {
      texto: responseData?.texto,
      estado: responseData?.estado,
      pergunta: responseData?.pergunta || 'N/A',
      carroSelecionado: carro,
      marcaSelecionada: carro.marca?.nome || null,
      modeloSelecionado: carro.modelo?.nome || null,
      motorSelecionado: carro.motor?.nome || null,
      diagnostico: diagnostico,
      solucao: solucao,
      explicacaoGeral: explicacaoGeral,
      explicacaoGeralNao: explicacaoGeralNao,
      como: como,
      evidencias: evidencias,
      triggeredRules: triggeredRules,
      diagnosticoConcluido: responseData?.diagnosticoConcluido || false,
    };

    try {
      const response = await fetch(
        `${getApiUrl()}/diagnostico/perguntaAnterior?perguntaAtual=${encodeURIComponent(perguntaAtual)}`,
        {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify(body),
        }
      );

      if (response.ok) {
        const jsonResponse = await response.text();
        const questionMatch = jsonResponse.match(/Question -> (.*?),/);
        const answerMatch = jsonResponse.match(/Answer -> (.*)/);

        const question = questionMatch ? questionMatch[1] : 'N/A';
        const answer = answerMatch ? answerMatch[1] : 'N/A';

        const formattedResponse = `Porque a pergunta anterior foi "${question}" e a resposta foi "${answer}"`;
        setResponseText(formattedResponse);
        setActiveButtonIndex(index);
      } else {
        console.error('Error:', response.statusText);
        setResponseText('Error: ' + response.statusText);
        setActiveButtonIndex(index);
      }
    } catch (error) {
      console.error('Fetch error:', error);
      setResponseText('Fetch error: ' + error.message);
      setActiveButtonIndex(index);
    }
  };

  return (
    <CardWrapper titulo="Diagnóstico Concluído">
      <Box sx={{ position: 'absolute', top: 16, right: 70 }}>
        <IconButton
          variant="contained"
          onClick={() =>
            generatePDF(carro, diagnostico, solucao, explicacaoGeral, explicacaoGeralNao, como, evidencias, triggeredRules)
          }
          sx={{
            color: 'red',
            backgroundColor: 'white',
            marginLeft: '10px',
            '&:hover': { backgroundColor: 'rgba(255, 0, 0, 0.1)' },
          }}
        >
          <PictureAsPdfIcon />
        </IconButton>
      </Box>

      <Accordion>
        <AccordionSummary expandIcon={<ExpandMoreIcon />}>
          <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left' }}>
            {carro.marca?.nome || 'N/A'}, {carro.modelo?.nome || 'N/A'}, {carro.motor?.nome || 'N/A'}
          </Typography>
          <Typography variant="h6" sx={{ color: 'text.secondary' }}>
            <i>Veja os componentes</i>
          </Typography>
        </AccordionSummary>
        <AccordionDetails>
          <Typography variant="h6" gutterBottom textAlign="left">
            Níveis ideais dos componentes
          </Typography>
          <List>
            {carro.componentes?.length > 0 ? (
              carro.componentes.map((componente, index) => (
                <ListItem key={index}>
                  {componente.nome}: {componente.valorMinimoIdeal} - {componente.valorMaximoIdeal} {componente.unidade}
                </ListItem>
              ))
            ) : (
              <ListItem>Nenhum componente encontrado.</ListItem>
            )}
          </List>
        </AccordionDetails>
      </Accordion>

      <Accordion defaultExpanded variant="outlined">
        <AccordionSummary expandIcon={<ExpandMoreIcon />} sx={{ color: 'lime' }}>
          <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left', color: 'inherit' }}>
            Diagnóstico Geral
          </Typography>
          <Typography variant="h6" sx={{ color: 'inherit' }}>
            {diagnostico}
          </Typography>
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
          <Typography variant="h6" sx={{ color: 'text.secondary' }}>
            <i>Entenda o diagnóstico</i>
          </Typography>
        </AccordionSummary>
        <AccordionDetails>
          <List>
            <ListItem>
              <Typography variant="h6" gutterBottom>
                Explicação Geral: {explicacaoGeral}
              </Typography>
            </ListItem>
            <ListItem>
              <Typography variant="h6" gutterBottom>
                Explicação Geral (Não): {explicacaoGeralNao}
              </Typography>
            </ListItem>

            <Grid container spacing={2} alignItems="center">
              <Grid item xs={12}>
                <Accordion variant="elevation">
                  <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                    <Typography variant="h6">Como chegou-se ao diagnóstico?</Typography>
                  </AccordionSummary>
                  <AccordionDetails>
                    <List>
                      {como.split('\n').map((line, index) => (
                        <ListItem key={index}>
                          <Typography variant="body1">{line}</Typography>
                        </ListItem>
                      ))}
                    </List>
                  </AccordionDetails>
                </Accordion>
              </Grid>

              <Grid item xs={12}>
                <Accordion variant="elevation">
                  <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                    <Typography variant="h6">Porquê estas evidências?</Typography>
                  </AccordionSummary>
                  <AccordionDetails>
                    <List>
                      {evidencias.length > 0 ? (
                        evidencias.slice(1).map((evidencia, index) => (
                          <ListItem key={index} sx={{ display: 'flex', justifyContent: 'space-between', width: '100%' }}>
                            <Grid container spacing={2} alignItems="center">
                              <Grid item xs>
                                {evidencia.fact}
                                {activeButtonIndex === index && responseText && (
                                  <Typography variant="body2" sx={{ mt: 1, color: 'lime' }}>
                                    {responseText}
                                  </Typography>
                                )}
                              </Grid>
                              <Grid item>
                                <Button
                                  variant="contained"
                                  endIcon={<Help />}
                                  onClick={() => handlePorqueClick(evidencia.fact, index)}
                                >
                                  Porque
                                </Button>
                              </Grid>
                            </Grid>
                          </ListItem>
                        ))
                      ) : (
                        <ListItem>Nenhuma evidência encontrada.</ListItem>
                      )}
                    </List>
                  </AccordionDetails>
                </Accordion>
              </Grid>

              <Grid item xs={12}>
                <Accordion variant="elevation">
                  <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                    <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left' }}>
                      <em>Porque Não</em> Outro Diagnóstico?
                    </Typography>
                  </AccordionSummary>
                  <AccordionDetails>
                    <FormControl fullWidth>
                      <InputLabel>Todas falhas</InputLabel>
                      <Select value={falha} onChange={handleFalhasChange}>
                        <MenuItem value="">
                          <em>Selecione a Falha</em>
                        </MenuItem>
                        {falhas.map((f, idx) => (
                          <MenuItem key={idx} value={f}>
                            {f}
                          </MenuItem>
                        ))}
                      </Select>
                    </FormControl>
  
                    {falha && falhaDetalhes && (
                      <Box sx={{ mt: 2 }}>
                        {falhaDetalhes.map((detalhe, index) => (
                          <Box key={index} sx={{ mt: 2, pl: 2, borderLeft: '3px solid lime' }}>
                            <Typography variant="body2">{detalhe}</Typography>
                          </Box>
                        ))}
                      </Box>
                    )}
                  </AccordionDetails>
                </Accordion>
              </Grid>

            </Grid>
          </List>
        </AccordionDetails>
      </Accordion>

      <Accordion headerstyle={{ width: '100%' }}>
        <AccordionSummary expandIcon={<ExpandMoreIcon />}>
          <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left' }}>
            Regras acionadas
          </Typography>
          <Typography variant="h6" sx={{ color: 'text.secondary' }}>
            <i>Mais detalhes de diagnóstico</i>
          </Typography>
        </AccordionSummary>
        <AccordionDetails>
          <List>
            {triggeredRules.length > 0 ? (
              triggeredRules.map((regra, index) => (
                <ListItem key={index}>
                  <Typography variant="body1">{regra}</Typography>
                </ListItem>
              ))
            ) : (
              <ListItem>Nenhuma regra acionada.</ListItem>
            )}
          </List>
        </AccordionDetails>
      </Accordion>
    </CardWrapper>
  );
};

export default ConclusionPage;