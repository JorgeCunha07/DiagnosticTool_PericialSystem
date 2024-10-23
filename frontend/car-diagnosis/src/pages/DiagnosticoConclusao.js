import { Box, Card, CardContent, Container, Typography } from '@mui/material';
import React from 'react';
import useConclusao from '../hooks/useConclusao';

const ConclusionPage = () => {
  const { 
    diagnostico, 
    solucao, 
    explicacaoGeral, 
    explicacaoGeralNao, 
    como, 
    evidencias, 
    triggeredRules, 
    responseData 
  } = useConclusao();

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

            <Typography variant="h6" gutterBottom>
              Diagnóstico: {diagnostico}
            </Typography>

            <Typography variant="h6" gutterBottom>
              Solução: {solucao}
            </Typography>

            <Typography variant="h6" gutterBottom>
              Explicação Geral: {explicacaoGeral}
            </Typography>

            <Typography variant="h6" gutterBottom>
              Explicação Geral (Não): {explicacaoGeralNao}
            </Typography>

            <Typography variant="h6" gutterBottom>
              Como: {como}
            </Typography>

            <Typography variant="h6" gutterBottom>
              Evidências:
            </Typography>
            {evidencias.length > 0 ? (
              evidencias.map((evidencia, index) => (
                <Typography key={index} variant="body1">
                  {evidencia.fact}: {evidencia.value}
                </Typography>
              ))
            ) : (
              <Typography variant="body2">Nenhuma evidência encontrada.</Typography>
            )}

            <Typography variant="h6" gutterBottom>
              Regras Acionadas:
            </Typography>
            {triggeredRules.length > 0 ? (
              triggeredRules.map((rule, index) => (
                <Typography key={index} variant="body1">
                  {rule}
                </Typography>
              ))
            ) : (
              <Typography variant="body2">Nenhuma regra acionada.</Typography>
            )}
          </CardContent>
          
          {/* JSON Response */}
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

export default ConclusionPage;
