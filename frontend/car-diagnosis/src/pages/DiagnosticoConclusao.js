import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import Help from '@mui/icons-material/Help';
import { Accordion, AccordionDetails, AccordionSummary, Box, Button, Card, CardContent, Container, Grid, List, ListItem, Typography } from '@mui/material';
import React from 'react';
import { useLocation } from 'react-router-dom';

const ConclusionPage = () => {
  // Extracting responseData using useLocation
  const location = useLocation();
  const { responseData } = location.state || {};

  // Extracting necessary data from responseData
  const diagnostico = responseData?.diagnostico || 'N/A';
  const solucao = responseData?.solucao || 'N/A';
  const explicacaoGeral = responseData?.explicacaoGeral || 'N/A';
  const explicacaoGeralNao = responseData?.explicacaoGeralNao || 'N/A';
  const como = responseData?.como || 'N/A';
  const evidencias = responseData?.evidencias || [];
  const triggeredRules = responseData?.triggeredRules || [];
  const carro = responseData?.carroSelecionado || {};

  const [expanded, setExpanded] = React.useState(false);

  const handleExpansion = () => {
    setExpanded((prevExpanded) => !prevExpanded);
  };

  return (
    <Container>
      <Box
        display="flex"
        justifyContent="center"
        alignItems="center"
        minHeight="100vh"
      >
        <Card>
          <CardContent>
            <Typography variant="h4" gutterBottom>
              Diagnóstico Concluído
            </Typography>
            <Accordion >
              <AccordionSummary
                expandIcon={<ExpandMoreIcon />}
              >
                <Typography variant="h6" sx={{ width:'400px', flexShrink: 0, textAlign: 'left' }}>
                  {carro.marca?.nome || 'N/A'}, {carro.modelo?.nome || 'N/A'}, {carro.motor?.nome || 'N/A'}
                </Typography>
                <Typography variant="h6" sx={{ color: 'text.secondary' }}>Veja os componentes</Typography>
              </AccordionSummary>
              <AccordionDetails>
                <Typography variant="h6" gutterBottom textAlign={'left'}>
                  Níveis ideais do componentes
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
              <AccordionSummary
                expandIcon={<ExpandMoreIcon />}
              >
                <Typography variant="h6" sx={{ width:'400px', flexShrink: 0, textAlign: 'left' }}>
                  Diagnóstico Geral
                </Typography>
                <Typography variant="h6" sx={{ color: 'text.secondary' }}>{diagnostico}</Typography>
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
              <AccordionSummary
                expandIcon={<ExpandMoreIcon />}
              >
                <Typography variant="h6" sx={{ width:'400px', flexShrink: 0, textAlign: 'left' }}>
                  Explicações
                </Typography>
                <Typography variant="h6" sx={{ color: 'text.secondary' }}>Entenda o diagnóstico</Typography>
              </AccordionSummary>
              <AccordionDetails>
                <Typography>
                  <List>

                    <ListItem>
                      <Typography variant="h6" gutterBottom>
                        Explicação Geral (Porque geral): {explicacaoGeral}
                      </Typography>
                    </ListItem>

                    <ListItem>
                      <Typography variant="h6" gutterBottom>
                        Explicação Geral (Porque não geral): {explicacaoGeralNao}
                      </Typography>
                    </ListItem>

                    <Grid container spacing={2} alignItems="center">
                      <Grid item>
                      <Accordion variant="elevation">
                        <AccordionSummary
                          expandIcon={<ExpandMoreIcon />}
                        >
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
                      <Grid item>
                      <Accordion variant="elevation">
                        <AccordionSummary
                          expandIcon={<ExpandMoreIcon />}
                        >
                          <Typography variant='h6'>Porquê esta evidência?</Typography>
                        </AccordionSummary>
                        <AccordionDetails>
                          {/* Processo de inferência baseado nas seguintes evidências: */}
                          <List>
                            {evidencias.length > 0 ? (
                              evidencias
                                .slice(1) // Não mostra a primeira evidência, == "O carro apresenta algum problema?"
                                .map((evidencia, index) => (
                                  <ListItem key={index} sx={{ display: 'flex', justifyContent: 'space-between', width: '100%' }}>
                                    <Grid container spacing={2} alignItems="center">
                                      <Grid item xs>
                                        {evidencia.fact} {/* {evidencia.value} */}
                                      </Grid>
                                      <Grid item>
                                        <Button variant="contained" endIcon={<Help />}>Porque</Button>
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
                </Typography>
              </AccordionDetails>
            </Accordion>
            
            <Accordion headerstyle={{width: "100%"}} >
                        <AccordionSummary
                          expandIcon={<ExpandMoreIcon />}
                        >
                          <Typography variant="h6" sx={{ width:'400px', flexShrink: 0, textAlign: 'left' }}>
                            Mais detalhes
                          </Typography>
                          <Typography variant="h6" sx={{ color: 'text.secondary' }}>Regras acionadas</Typography>
                        </AccordionSummary>
                        <AccordionDetails>
                          <List>
                            {triggeredRules.length > 0 ? (
                              triggeredRules.map((rule, index) => (
                                <ListItem key={index}>
                                  {rule}
                                </ListItem>
                              ))
                            ) : (
                              <ListItem>Nenhuma regra acionada.</ListItem>
                            )}
                          </List>
                        </AccordionDetails>
                      </Accordion>
          </CardContent>

          {/* <CardContent>
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
          </CardContent> */}
        </Card>
      </Box>
    </Container>
  );
};

export default ConclusionPage;
