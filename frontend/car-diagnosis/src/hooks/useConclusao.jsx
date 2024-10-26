import { useState } from 'react';
//import API_URL from '../config/apiConfig';
import { getApiUrl } from '../config/apiConfig';

const useConclusao = (responseData, carro, diagnostico, solucao, explicacaoGeral, explicacaoGeralNao, como, evidencias, triggeredRules) => {
  const [responseText, setResponseText] = useState('');
  const [activeButtonIndex, setActiveButtonIndex] = useState(null);

  const handlePorqueClick = async (fact, index) => {
    const perguntaAtual = fact;

    const body = {
      texto: responseData?.texto,
      estado: responseData?.estado,
      pergunta: responseData?.pergunta || 'N/A',
      carroSelecionado: carro,
      marcaSelecionada: carro.marca?.nome || null,
      modeloSelecionado: carro.modelo?.nome || null,
      motorSelecionado: carro.motor?.nome || null,
      diagnostico: diagnostico,
      solucao: solucao,
      explicacaoGeral: explicacaoGeral,
      explicacaoGeralNao: explicacaoGeralNao,
      como: como,
      evidencias: evidencias,
      triggeredRules: triggeredRules,
      diagnosticoConcluido: responseData?.diagnosticoConcluido || false,
    };

    try {
      //const response = await fetch(`${API_URL}/diagnostico/perguntaAnterior?perguntaAtual=${encodeURIComponent(perguntaAtual)}`, {
      const response = await fetch(`${getApiUrl()}/diagnostico/perguntaAnterior?perguntaAtual=${encodeURIComponent(perguntaAtual)}`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(body),
      });

      if (response.ok) {
        const jsonResponse = await response.text();
        const questionMatch = jsonResponse.match(/Question -> (.*?),/);
        const answerMatch = jsonResponse.match(/Answer -> (.*)/);

        const question = questionMatch ? questionMatch[1] : 'N/A';
        const answer = answerMatch ? answerMatch[1] : 'N/A';

        const formattedResponse = `Porque a pergunta anterior foi "${question}" e a resposta foi "${answer}"`;
        setResponseText(formattedResponse);
        setActiveButtonIndex(index);
      } else {
        console.error('Error:', response.statusText);
        setResponseText('Error: ' + response.statusText);
        setActiveButtonIndex(index);
      }
    } catch (error) {
      console.error('Fetch error:', error);
      setResponseText('Fetch error: ' + error.message);
      setActiveButtonIndex(index);
    }
  };

  return { responseText, activeButtonIndex, handlePorqueClick };
};

export default useConclusao;
