import axios from 'axios';
import { useEffect, useState } from 'react';

const API_URL = 'http://localhost:8080/api';

const useSelecao = () => {
  const [carData, setCarData] = useState([]);
  const [marca, setMarca] = useState('');
  const [modelo, setModelo] = useState('');
  const [motor, setMotor] = useState('');
  const [modelos, setModelos] = useState([]);
  const [motores, setMotores] = useState([]);
  const [componentes, setComponentes] = useState([]);
  const [error, setError] = useState(null);


  // Fetch data
  useEffect(() => {
    const fetchCarData = async () => {
      try {
        const response = await axios.get(API_URL + '/carros');
        setCarData(response.data);
      } catch (error) {
        console.error("Error fetching data from API", error);
      }
    };
    fetchCarData();
  }, []);


  // Get unique marcas
  const marcas = [...new Set(carData.map(car => car.marca.nome))];

  const handleMarcaChange = (e) => {
    const selectedMarca = e.target.value;
    setMarca(selectedMarca);
    setModelo('');
    setMotor('');

    const filteredModelos = carData.filter(car => car.marca.nome === selectedMarca);
    const uniqueModelos = [...new Set(filteredModelos.map(car => car.modelo.nome))];
    setModelos(uniqueModelos);
    setMotores([]);
    setComponentes([]);
  };

  const handleModeloChange = (e) => {
    const selectedModelo = e.target.value;
    setModelo(selectedModelo);
    setMotor('');

    const filteredMotores = carData.filter(car => car.marca.nome === marca && car.modelo.nome === selectedModelo);
    const uniqueMotores = [...new Set(filteredMotores.map(car => car.motor.nome))];
    setMotores(uniqueMotores);
    setComponentes([]);
  };

  const handleMotorChange = (e) => {
    const selectedMotor = e.target.value;
    setMotor(selectedMotor);

    const filteredCar = carData.find(car => car.marca.nome === marca && car.modelo.nome === modelo && car.motor.nome === selectedMotor);
    setComponentes(filteredCar ? filteredCar.componentes : []);
  };

  const iniciarDiagnostico = async (navigate) => {
    const body = {
      marca: { nome: marca },
      modelo: { nome: modelo },
      motor: { nome: motor },
      componentes: componentes
    };

    try {
      const response = await axios.post(API_URL + '/diagnostico/iniciar', body);
      // Navigate to the diagnostic page and pass the response data
      navigate('/diagnostico', { state: { diagnosticoData: response.data } });
    } catch (err) {
      setError('Falha ao iniciar diagnostico');
    }
  };

  return {
    marcas,
    marca,
    modelo,
    motor,
    modelos,
    motores,
    componentes,
    error,
    handleMarcaChange,
    handleModeloChange,
    handleMotorChange,
    iniciarDiagnostico,
  };
};

export default useSelecao;
