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
  const posicao_entrelinhas = 9;
  const lineWidth = largura_pagina - margem_esquerda;

  const fontsize_body = 12;
  const fontsize_h1 = 18;
  const fontsize_h2 = 14;
  const fontsize_small = 10;

  const currentDate = new Date();
  const timestamp = currentDate.toLocaleString(); 
  let paginaAtual = 1;

  const x_position = (() => {
    return (identar) => (typeof identar === 'number' ? (identar * (margem_esquerda / 2) + margem_esquerda) : margem_esquerda);
  })();

  const y_position = (() => {
    let y = margem_topo_pagina;

    return (posicao_diferente) => {
      if (y >= altura_pagina - margem_baixo_pagina) {
        paginaAtual++;
        doc.addPage();
        addFooter();
        y = margem_topo_pagina;
      }
      y += typeof posicao_diferente === 'number' ? posicao_diferente : posicao_entrelinhas;
      return y;
    };
  })();

  const drawHorizontalLine = () => {
    const y_line = y_position();
    doc.line(x_position(), y_line, lineWidth, y_line);
  };

  const addFooter = () => {
    doc.setFontSize(fontsize_small);
    const footerText = `Página ${paginaAtual}`;
    const textWidth = doc.getTextWidth(footerText);
    const xPosition = lineWidth - textWidth;
    doc.text(footerText, xPosition, altura_pagina - margem_baixo_pagina + 10);
    doc.setFontSize(fontsize_body);
  };

  const addWrappedText = (text, fontsize=fontsize_body, identation = 0) => {
    //const wrappedText = doc.splitTextToSize(text, lineWidth - (4*margem_esquerda));
    const wrappedText = doc.splitTextToSize(text, lineWidth - (4*margem_esquerda) - (identation*(margem_esquerda/2)));
    wrappedText.forEach((line) => {
      doc.setFontSize(fontsize);
      doc.text(line, x_position(identation), y_position());
    });
  };

  addFooter();

  //// INICIO

  // Titulo
  addWrappedText('Diagnóstico Veicular Innov8', fontsize_h1);

  addWrappedText(`Data: ${timestamp}`, fontsize_small);

  //addWrappedText('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut molestie luctus mi vitae porta. Phasellus a urna semper, vehicula justo in, lacinia sapien. Fusce dictum interdum augue eu varius. Praesent rhoncus mauris sit amet libero suscipit, ut elementum ipsum pellentesque. Vivamus volutpat pulvinar arcu. Vestibulum hendrerit efficitur orci, vel ornare velit hendrerit vitae. Morbi iaculis lorem id eleifend vulputate. Pellentesque placerat ac odio vitae hendrerit. Donec pharetra non dui et condimentum.', fontsize_body, 2);

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

            if (responseText) {
              addWrappedText(`${responseText[0]}`, fontsize_body, 3);
              
              // nem todas respostas porque tem um motivo. exemplo: primeiro porque.
              if(responseText[1])
                addWrappedText(`${responseText[1]}`, fontsize_body, 3);
            }
          }
      }
      });
    }
  }

  if (falhaDetalhes && falha) {
    drawHorizontalLine();
    addWrappedText(`Porque não: ${falha}`, fontsize_h2);
  
    if (falhaResponseTextVisible) {

      falhaDetalhes.forEach((detalhe) => {
        addWrappedText(detalhe.porque, fontsize_body, 1);
        addWrappedText(detalhe.motivo, fontsize_body, 2);
      })
    }
  }

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

//// FIM

  const pdfData = doc.output('bloburl');
  window.open(pdfData, '_blank');
};
