import { Route, BrowserRouter as Router, Routes } from 'react-router-dom';
import './App.css';
import Diagnostico from './pages/Diagnostico';
import SelecaoCarro from './pages/SelecaoCarro';



function App() {
  return (
    <div className="App">
      <Router>
        <Routes>
        <Route path="/" element={<SelecaoCarro />} />
        <Route path="/diagnostico" element={<Diagnostico />} />
        </Routes>
      </Router>
    </div>
  );
}

export default App;
