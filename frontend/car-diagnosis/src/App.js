import { createTheme, CssBaseline, ThemeProvider } from '@mui/material';
import React from 'react';
import { Route, BrowserRouter as Router, Routes } from 'react-router-dom';
import './App.css';
import DiagnosticoCarro from './pages/DiagnosticoCarro';
import DiagnosticoConclusao from './pages/DiagnosticoConclusao';
import DiagnosticoErro from './pages/DiagnosticoErro';
import Home from './pages/Home';
import SelecaoCarro from './pages/SelecaoCarro';


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

function App() {
  return (
    <ThemeProvider theme={darkTheme}>
    <CssBaseline />
      <div className="App">
        <Router>
          <Routes>
            {/* <Route path="/" element={<IdentificacaoUsuario />} /> */}
            <Route path="/" element={<Home />} />
            <Route path="/selecao" element={<SelecaoCarro />} />
            <Route path="/diagnostico" element={<DiagnosticoCarro />} />
            <Route path="/error" element={<DiagnosticoErro />} />
            <Route path="/conclusao" element={<DiagnosticoConclusao />} />
          </Routes>
        </Router>
      </div>
    </ThemeProvider>
  );
}

export default App;
