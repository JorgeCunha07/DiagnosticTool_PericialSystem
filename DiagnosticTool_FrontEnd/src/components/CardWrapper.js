import HomeIcon from '@mui/icons-material/Home';
import { Box, Card, CardContent, Container, IconButton, Typography } from '@mui/material';
import React from 'react';
import { useNavigate } from 'react-router-dom';
import { getSistemaSelecionado } from '../config/apiConfig';

const CardWrapper = ({ titulo, children, pdf="false" }) => {
    
    const navigate = useNavigate();
    const sistemaSelecionado = getSistemaSelecionado();

    return (
        <>
        <Container
            sx={{ width: '1000px', margin: 'auto', padding: '20px' }}
        >
            <Box
                display="flex"
                justifyContent="center"
                alignItems="center"
                minHeight="100vh"
            >
                <Card
                    sx={{ position: 'relative', padding: 2, width: "900px" }}
                >
                    <Box
                        sx={{ position: 'absolute', top: 16, right: 16 }}
                    >
                        <IconButton
                            variant="contained"
                            onClick={() => navigate('/')}
                            color='primary'
                            sx={{ color: 'primary', backgroundColor: 'white' }}
                        >
                            <HomeIcon />
                        </IconButton>
                    </Box>
                    <CardContent padding={2}>
                        <Typography
                            variant="h5"
                            component="h1"
                            sx={{ marginBottom: '30px'}}
                            gutterBottom
                        >
                            {titulo}: {sistemaSelecionado}
                        </Typography>
                        {children}
                    </CardContent>
                </Card>
            </Box>
        </Container>
        </>
    );
};

export default CardWrapper;