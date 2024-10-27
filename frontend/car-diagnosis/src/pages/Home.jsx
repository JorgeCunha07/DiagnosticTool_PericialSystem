import { Box, ButtonBase, Card, CardContent, Container, Typography } from '@mui/material';
import React from 'react';
import { useNavigate } from 'react-router-dom';
import { setApiUrl } from '../config/apiConfig';

const Home = () => {
    const navigate = useNavigate();

    const handleApiSelection = (identifier) => {
        setApiUrl(identifier);
        navigate('/selecao');
    };

    const img_tamanho = "120px";

    return (
        <Container sx={{ width: '1000px', margin: 'auto', padding: '20px' }}>
            <Box display="flex" justifyContent="center" alignItems="center" minHeight="100vh">
                <Card sx={{ width: 500, height: 360, padding: 5 }}>
                    <CardContent>
                        <Typography
                            variant="h4"
                            component="h1"
                            sx={{
                                height: "80px"
                            }}
                            gutterBottom
                        >
                            Selecione o Sistema
                        </Typography>

                        <Box display="flex" justifyContent="center" gap={4}>
                            <Box display="flex" flexDirection="column" alignItems="center">
                                <ButtonBase
                                    onClick={() => handleApiSelection('Drools')}
                                    sx={{
                                        borderRadius: '50%',
                                        height: img_tamanho,
                                        width: img_tamanho,
                                        display: 'flex',
                                        position: 'relative',
                                        '&:hover': {
                                            //border: '5px solid black',
                                            filter: 'grayscale(100%)',
                                            transition: 'filter 0.3s ease, box-shadow 0.3s ease',
                                            boxShadow: '0px 10px 20px rgba(0, 0, 0, 0.5)',
                                        },
                                    }}
                                >
                                    <img
                                        src={require(`../assets/img/systems/Drools.png`)}
                                        alt="Drools"
                                        height={img_tamanho}
                                        style={{
                                            padding: "10px",
                                            borderRadius: '50%',
                                            objectFit: 'cover',
                                            transition: 'filter 0.3s ease',
                                        }}
                                    />
                                </ButtonBase>
                                <Typography variant="body1" sx={{ marginTop: 1 }}>
                                    Drools
                                </Typography>
                            </Box>

                            <Box display="flex" flexDirection="column" alignItems="center">
                                <ButtonBase
                                    onClick={() => handleApiSelection('PROLOG')}
                                    sx={{
                                        borderRadius: '50%',
                                        display: 'flex',
                                        position: 'relative',
                                        '&:hover': {
                                            //border: '5px solid black',
                                            filter: 'grayscale(100%) hue-rotate(210deg)',
                                            transition: 'filter 0.3s ease, box-shadow 0.3s ease',
                                            boxShadow: '0px 10px 20px rgba(0, 0, 0, 0.5)',
                                        },
                                    }}
                                >
                                    <img
                                        src={require(`../assets/img/systems/PROLOG.png`)}
                                        alt="PROLOG"
                                        height={img_tamanho}
                                        style={{
                                            padding: "10px",
                                            borderRadius: '50%',
                                            objectFit: 'cover',
                                            transition: 'filter 0.3s ease',
                                        }}
                                    />
                                </ButtonBase>
                                <Typography variant="body1" sx={{ marginTop: 1 }}>
                                    PROLOG
                                </Typography>
                            </Box>
                        </Box>
                    </CardContent>
                </Card>
            </Box>
        </Container>
    );
};

export default Home;
