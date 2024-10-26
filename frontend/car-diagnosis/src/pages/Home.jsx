import { Button, Container, Typography } from '@mui/material';
import React from 'react';
import { useNavigate } from 'react-router-dom';
import { setApiUrl } from '../config/apiConfig';

const Home = () => {
    const navigate = useNavigate();

    const handleApiSelection = (apiUrl) => {
        setApiUrl(apiUrl);
        navigate('/selecao'); // pagina de selecao do carro
    };
    
    return (
        <Container sx={{ width: '1000px', margin: 'auto', padding: '20px'}}>
            <Typography variant="h4" component="h1" gutterBottom>
                Selecione o Sistema
            </Typography>
            <Button
                variant="contained"
                color="primary"
                onClick={() => handleApiSelection('http://localhost:8080/api')} // Drools
                sx={{ margin: '10px' }}
            >
                Drools
            </Button>
            <Button
                variant="contained"
                color="secondary"
                onClick={() => handleApiSelection('https://localhost:4040')} // PROLOG
                sx={{ margin: '10px' }}
            >
                PROLOG
            </Button>
        </Container>
    );
};

export default Home;
