import jsPDF from 'jspdf';
import 'jspdf-autotable';

export const generatePDF = (
  carro,
  diagnostico,
  solucao,
  explicacaoGeral,
  explicacaoGeralNao,
  como,
  evidencias,
  triggeredRules,
  responseText,
  activeButtonIndex,
  responseTextVisible,
  falha,
  falhaDetalhes,
  falhaResponseTextVisible
) => {
  const doc = new jsPDF();
  const altura_pagina = doc.internal.pageSize.height;
  const largura_pagina = doc.internal.pageSize.width;
  const margem_esquerda = 10;
  const margem_topo_pagina = 10;
  const margem_baixo_pagina = 20;
  const lineWidth = largura_pagina - margem_esquerda * 2;  // Adjust width based on both left and right margins

  const fontsize_body = 12;
  const fontsize_h1 = 18;
  const fontsize_h2 = 14;
  const fontsize_small = 10;

  const currentDate = new Date();
  const timestamp = currentDate.toLocaleString(); 
  let paginaAtual = 1;

  const x_position = (() => {
    let x = margem_esquerda;
    return (identar) => (typeof identar === 'number' ? (x = identar * (margem_esquerda / 2) + margem_esquerda) : margem_esquerda);
  })();

  const y_position = (() => {
    let y = margem_topo_pagina;
    const posicao_inicial_entrelinhas = 9;

    return (posicao_diferente) => {
      if (y >= altura_pagina - margem_baixo_pagina) {
        paginaAtual++;
        doc.addPage();
        addFooter();
        y = margem_topo_pagina;
      }
      y += typeof posicao_diferente === 'number' ? posicao_diferente : posicao_inicial_entrelinhas;
      return y;
    };
  })();

  const drawHorizontalLine = () => {
    const y_line = y_position();
    doc.line(x_position(), y_line, largura_pagina - margem_esquerda, y_line);
  };

  const addFooter = () => {
    doc.setFontSize(10);
    const footerText = `Página ${paginaAtual}`;
    const textWidth = doc.getTextWidth(footerText);
    const xPosition = lineWidth - textWidth;
    doc.text(footerText, xPosition, altura_pagina - margem_baixo_pagina + 10);
    doc.setFontSize(12);
  };

  addFooter();

  const addWrappedText = (text, fontsize=fontsize_body, identation = 0) => {
    const wrappedText = doc.splitTextToSize(text, lineWidth - (identation * margem_esquerda));
    wrappedText.forEach((line) => {
      doc.setFontSize(fontsize);
      doc.text(line, x_position(identation), y_position());
    });
  };

  // Titulo
  addWrappedText('Diagnóstico Veicular Innov8', fontsize_h1);

  addWrappedText(`Data: ${timestamp}`, fontsize_small);

  drawHorizontalLine();

  addWrappedText('Diagnóstico Geral', fontsize_h2);
  addWrappedText(`Diagnóstico: ${diagnostico}`);
  addWrappedText(`Solução: ${solucao}`);

  if (explicacaoGeral) {
    drawHorizontalLine();
    
    addWrappedText('Explicações', fontsize_h2);
    
    addWrappedText('Explicação Geral:');
    addWrappedText(explicacaoGeral);
    addWrappedText('Explicação Geral (Não):');
    addWrappedText(explicacaoGeralNao);
  }

  if (como) {
    drawHorizontalLine();
    addWrappedText(`Como chegou-se ao diagnóstico:`, fontsize_h2);

    if (Array.isArray(como)) {
      como.forEach((evidencia, index) => {
        if (evidencia.pergunta) {
          addWrappedText(`${index + 1}. ${evidencia.pergunta}`, fontsize_body, 1);
          addWrappedText(`Resposta: ${evidencia.resposta}`, fontsize_body, 2);

          if (index === activeButtonIndex && responseTextVisible) {
            addWrappedText(`Porque esta evidência?`, fontsize_body, 2);
            responseText.split('\n').forEach((line) => addWrappedText(line, fontsize_body, 3));
          }
      }
      });
    }
  }

  if (falhaDetalhes && falha) {
    drawHorizontalLine();
    addWrappedText(`Porque não: ${falha}`, fontsize_h2);
  
    if (falhaResponseTextVisible) {
      if (Array.isArray(falhaDetalhes)) {
        falhaDetalhes.forEach((detalhe) => {
          addWrappedText(`${detalhe.explicacao} ${detalhe.regra_id}:`, fontsize_body, 1);
          detalhe.detalhes.forEach((det) => {
            addWrappedText(`${det.explicacao} - ${det.premissa} ${det.condicao}`, fontsize_body, 2);
          });
        });
      } else {
        addWrappedText(falhaDetalhes.explicacao, fontsize_body, 1);
      }
    }
  }

  // Triggered Rules
  if (triggeredRules) {
    drawHorizontalLine();
    addWrappedText('Regras acionadas', fontsize_h2);
  
    if (triggeredRules.length > 0) {
      triggeredRules.forEach((regra, index) => {
        addWrappedText(`${index + 1}. ${regra}`, fontsize_body, 1);
      });
    } else {
      addWrappedText('Nenhuma regra acionada.', fontsize_body, 1);
    }
  }

  const pdfData = doc.output('bloburl');
  window.open(pdfData, '_blank');
};
