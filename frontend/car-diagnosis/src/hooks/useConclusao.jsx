import { useLocation } from 'react-router-dom';

const useConclusao = () => {
  const location = useLocation();
  const { responseData } = location.state || {};

  const diagnostico = responseData.diagnostico || 'N/A';
  const solucao = responseData.solucao || 'N/A';
  const explicacaoGeral = responseData.explicacaoGeral || 'N/A';
  const explicacaoGeralNao = responseData.explicacaoGeralNao || 'N/A';
  const como = responseData.como || 'N/A';
  const evidencias = responseData.evidencias || [];
  const triggeredRules = responseData.triggeredRules || [];

  return {
    diagnostico,
    solucao,
    explicacaoGeral,
    explicacaoGeralNao,
    como,
    evidencias,
    triggeredRules,
    responseData
  };
};

export default useConclusao;