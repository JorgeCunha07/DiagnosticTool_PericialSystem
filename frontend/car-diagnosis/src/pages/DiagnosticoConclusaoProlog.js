import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import Help from '@mui/icons-material/Help';
import HomeIcon from '@mui/icons-material/Home';
import PictureAsPdfIcon from '@mui/icons-material/PictureAsPdf';
import { Accordion, AccordionDetails, AccordionSummary, Box, Button, Card, CardContent, Container, Grid, IconButton, List, ListItem, Typography } from '@mui/material';
import React from 'react';
import { useLocation, useNavigate } from 'react-router-dom';
import { getSistemaSelecionado } from '../config/apiConfig';
import useConclusao from '../hooks/useConclusao';
import { generatePDF } from '../utils/pdfConclusao';

const DiagnosticoConclusaoProlog = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const sistemaSelecionado = getSistemaSelecionado();
  const { responseData } = location.state || {};

  const diagnostico = responseData?.diagnostico || 'N/A';
  const solucao = responseData?.solucao || 'N/A';
  const explicacaoGeral = responseData?.explicacaoGeral || 'N/A';
  const explicacaoGeralNao = responseData?.explicacaoGeralNao || 'N/A';
  const como = responseData?.como || 'N/A';
  const evidencias = responseData?.evidencias || [];
  const triggeredRules = responseData?.triggeredRules || [];
  const carro = responseData?.carroSelecionado || {};

  const { responseText, activeButtonIndex, handlePorqueClick } = useConclusao(
    responseData,
    carro,
    diagnostico,
    solucao,
    explicacaoGeral,
    explicacaoGeralNao,
    como,
    evidencias,
    triggeredRules
  );

  return (
    <Container>
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="100vh">
      <Card sx={{ position: 'relative', padding: 2 }}>
        <Box sx={{ position: 'absolute', top: 16, right: 16}}>
            <IconButton
              variant="contained"
              onClick={()=>navigate('/')}
              color='primary'
              sx={{ color: 'primary', backgroundColor: 'white'}}
            ><HomeIcon /></IconButton> 
            <IconButton
              variant="contained"
              onClick={() =>
                generatePDF(carro, diagnostico, solucao, explicacaoGeral, explicacaoGeralNao, como, evidencias, triggeredRules)
              }
              sx={{ color: 'red', backgroundColor: 'white', marginLeft:'10px', '&:hover': {
                  backgroundColor: 'rgba(255, 0, 0, 0.1)',
                },
              }}
            >
            {/* <IconButton
              variant="contained"
              color='primary'
              onClick={() =>
                generatePDF(carro, diagnostico, solucao, explicacaoGeral, explicacaoGeralNao, como, evidencias, triggeredRules)
              }
            > */}
              <PictureAsPdfIcon />
            </IconButton>
          </Box>
          <CardContent>
            <Typography variant="h4" gutterBottom>
              Diagnóstico Concluído: {sistemaSelecionado}
            </Typography>
            <Accordion>
              <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left' }}>
                  {carro.marca?.nome || 'N/A'}, {carro.modelo?.nome || 'N/A'}, {carro.motor?.nome || 'N/A'}
                </Typography>
                <Typography variant="h6" sx={{ color: 'text.secondary' }}><i>Veja os componentes</i></Typography>
              </AccordionSummary>
              <AccordionDetails>
                <Typography variant="h6" gutterBottom textAlign={'left'}>
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
              <AccordionSummary expandIcon={<ExpandMoreIcon />} sx={{  color: 'lime' }}>
                <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left', color: 'inherit' }}>
                  Diagnóstico Geral
                </Typography>
                <Typography variant="h6" sx={{ color: 'inherit' }}>{diagnostico}</Typography>
              </AccordionSummary>
              <AccordionDetails>
                <Typography>
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
                </Typography>
              </AccordionDetails>
            </Accordion>

            <Accordion defaultExpanded variant='outlined'>
              <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left' }}>
                  Explicações
                </Typography>
                <Typography variant="h6" sx={{ color: 'text.secondary' }}><i>Entenda o diagnóstico</i></Typography>
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
                          <Typography variant='h6'>Como chegou-se ao diagnóstico?</Typography>
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
                          <Typography variant='h6'>Porquê estas evidências?</Typography>
                        </AccordionSummary>
                        <AccordionDetails>
                          <List>
                            {evidencias.length > 0 ? (
                              evidencias
                                .slice(1) // Pula a primeira evidência, que é = "O carro apresenta algum problema?"
                                .map((evidencia, index) => (
                                  <ListItem key={index} sx={{ display: 'flex', justifyContent: 'space-between', width: '100%' }}>
                                    <Grid container spacing={2} alignItems="center">
                                      <Grid item xs>
                                        {evidencia.fact} 
                                        {activeButtonIndex === index && responseText && (
                                          <Typography variant="body2" sx={{ mt: 1 }} sx={{ color: 'lime' }}>
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
                  </Grid>
                </List>
              </AccordionDetails>
            </Accordion>

            <Accordion headerstyle={{ width: "100%" }} >
              <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                <Typography variant="h6" sx={{ width: '400px', flexShrink: 0, textAlign: 'left' }}>
                  Regras acionadas
                </Typography>
                <Typography variant="h6" sx={{ color: 'text.secondary' }}><i>Mais detalhes de diagnóstico</i></Typography>
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
              {JSON.stringify(responseData, null, 2)}
            </Box>
          </CardContent>
        </Card>
      </Box>
    </Container>
  );
};

export default DiagnosticoConclusaoProlog;