import { Box, Card, CardContent, Container, Typography } from "@mui/material";
import React from "react";
import { useLocation } from "react-router-dom";

const DiagnosticoErro = () => {
  const location = useLocation();
  const { responseData } = location.state || {};

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
              Error: Dados inválidos
            </Typography>

            <Typography variant="body1" gutterBottom>
              Resposta do servidor:
            </Typography>
            <Box
              component="pre"
              sx={{
                whiteSpace: "pre-wrap",
                wordWrap: "break-word",
                padding: "10px",
                background: "black",
                textAlign: "left",
              }}
            >
              {JSON.stringify(responseData, null, 2)} {/* Pretty-print JSON */}
            </Box>
          </CardContent>
        </Card>
      </Box>
    </Container>
  );
};

export default DiagnosticoErro;
