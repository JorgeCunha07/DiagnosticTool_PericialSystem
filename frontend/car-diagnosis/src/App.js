import { createTheme, CssBaseline, ThemeProvider } from '@mui/material';
import React from 'react';
import { Route, Routes, useLocation } from 'react-router-dom';
import { CSSTransition, TransitionGroup } from 'react-transition-group';
import './App.css';
import DiagnosticoCarro from './pages/DiagnosticoCarro';
import DiagnosticoCarroProlog from './pages/DiagnosticoCarroProlog';
import DiagnosticoConclusao from './pages/DiagnosticoConclusao';
import DiagnosticoConclusaoProlog from "./pages/DiagnosticoConclusaoProlog";
import DiagnosticoErro from './pages/DiagnosticoErro';
import Home from './pages/Home';
import SelecaoCarro from './pages/SelecaoCarro';
import './styles/FadeTransition.css';

const darkTheme = createTheme({
  palette: {
    mode: 'dark',
    primary: {
      main: '#90caf9',
    },
    background: {
      default: '#121212',
      paper: '#1e1e1e',
    },
    text: {
      primary: '#ffffff',
    },
  },
});

const App = () => {
  const location = useLocation();

  return (
    <TransitionGroup>
      <CSSTransition key={location.key} classNames="fade" timeout={300}>
        <ThemeProvider theme={darkTheme}>
          <CssBaseline />
          <Routes location={location}>
            {/* <Route path="/" element={<IdentificacaoUsuario />} /> */}
            <Route path="/" element={<Home />} />
            <Route path="/selecao" element={<SelecaoCarro />} />
            <Route path="/diagnostico" element={<DiagnosticoCarro />} />
            <Route path="/diagnostico/prolog" element={<DiagnosticoCarroProlog />} />
            <Route path="/conclusao" element={<DiagnosticoConclusao />} />
            <Route path="/conclusao/prolog" element={<DiagnosticoConclusaoProlog />} />
            <Route path="/error" element={<DiagnosticoErro />} />
          </Routes>
        </ThemeProvider>
      </CSSTransition>
    </TransitionGroup>
  );
}

export default App;