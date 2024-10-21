import { createTheme, CssBaseline, ThemeProvider } from '@mui/material';
import React from 'react';
import { Route, BrowserRouter as Router, Routes } from 'react-router-dom';
import './App.css';
import DiagnosticoCarro from './pages/DiagnosticoCarro';
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
            <Route path="/" element={<SelecaoCarro />} />
            <Route path="/diagnostico" element={<DiagnosticoCarro />} />
            {/* <Route path="/conclusao" element={<Conclusao />} /> */}
          </Routes>
        </Router>
      </div>
    </ThemeProvider>
  );
}

export default App;
