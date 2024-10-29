import jsPDF from 'jspdf';
import 'jspdf-autotable';

// generatePDF(carro=null, diagnostico, solucao, explicacaoGeral=null, explicacaoGeralNao=null, como, evidencias=null, triggeredRules=null)

export const generatePDF = (carro, diagnostico, solucao, explicacaoGeral, explicacaoGeralNao, como, evidencias, triggeredRules) => {
  const doc = new jsPDF();
  const altura_pagina = doc.internal.pageSize.height;  // 298 eh o tamanho A4
  const margem_esquerda = 10;
  const margem_topo_pagina = 10;
  const margem_baixo_pagina = 20;
  let paginaAtual = 1;

  const x_position = (function () {
    let x = margem_esquerda;
    return function (identar) {
      if (typeof identar == 'number'){
        return x = (identar*(margem_esquerda/2)) + margem_esquerda;
      }
      else {
        return x = margem_esquerda;
      }
    };
  })();

  const y_position = (function () {
    let y = margem_topo_pagina;
    let posicao_inicial_entrelinhas = 9;
    let posicao_entre_linhas = posicao_inicial_entrelinhas;
  
    return function (posicao_diferente) {
      if (y >= (altura_pagina - margem_baixo_pagina)) {
        paginaAtual++;
        doc.addPage();
        addFooter();
        y = margem_topo_pagina;
      }
      if (typeof posicao_diferente === 'number') {
        posicao_entre_linhas = posicao_diferente;
      }
      else {
        posicao_entre_linhas = posicao_inicial_entrelinhas;
      }
      y += posicao_entre_linhas;
      return y;
    };
  })();

  const lineBreaker = (text) => {
    if (text instanceof String){
      return text.split('\n');
    }
  };

  const addFooter = () => {
    doc.setFontSize(10);
    const footerText = `Página ${paginaAtual}`;
    const textWidth = doc.getTextWidth(footerText);
    const xPosition = doc.internal.pageSize.width - textWidth - margem_esquerda;
    doc.text(footerText, xPosition, altura_pagina - margem_baixo_pagina + 10);
    doc.setFontSize(12);

    // Alinhamento a esquerda
  //   doc.text(`Página ${paginaAtual}`, x_position(), altura_pagina - margem_baixo_pagina + 10, { align: 'left' });
  };

  addFooter();
  doc.setFontSize(18);
  doc.text('Diagnóstico Veicular Innov8', x_position(), y_position());

  // Timestamp
  const currentDate = new Date();
  const timestamp = currentDate.toLocaleString(); 
  doc.setFontSize(10);
  doc.text(`Data: ${timestamp}`, x_position(), y_position());
  
  let y_line = y_position();
  doc.line(x_position(), y_line, 200, y_line);

  if(carro) {
    doc.setFontSize(14);
    doc.text(`Veículo Analisado`, x_position(), y_position());
    doc.setFontSize(12);
    doc.text(`Marca: ${carro.marca?.nome || 'N/A'}`, x_position(), y_position());
    doc.text(`Modelo: ${carro.modelo?.nome || 'N/A'}`, x_position(), y_position());
    doc.text(`Motor: ${carro.motor?.nome || 'N/A'}`, x_position(), y_position());
    doc.text('Níveis ideais dos componentes para este veículo', x_position(), y_position());
  

  doc.setFontSize(10);
  if (carro.componentes?.length > 0) {
    const column1X = x_position(1); // X para coluna esquerda
    const column2X = 110; // X para coluna direita
    const startY =  y_position();
    let yPosition = startY;

    carro.componentes.forEach((componente, index) => {
      const text = `${componente.nome}: ${componente.valorMinimoIdeal} - ${componente.valorMaximoIdeal} ${componente.unidade}`;

      if (index % 2 === 0) {
        // Coluna esquerda
        doc.text(text, column1X, yPosition);
      } else {
        // Coluna direita
        doc.text(text, column2X, yPosition);
        yPosition = y_position(7);
      }
    });

    if (carro.componentes.length % 2 !== 0) {
      yPosition = y_position(7);
    }
  } else {
    doc.text('Nenhum componente encontrado.', x_position(1), y_position());
  }

}
// UMA COLUNA
  // if (carro.componentes?.length > 0) {
  //   carro.componentes.forEach((componente, index) => {
  //     doc.text(`${componente.nome}: ${componente.valorMinimoIdeal} - ${componente.valorMaximoIdeal} ${componente.unidade}`, 10, 60 + index * 10);
  //   });
  // } else {
  //   doc.text('Nenhum componente encontrado.', 10, 60);
  // }

  y_line = y_position(0);
  doc.line(x_position(), y_line, 200, y_line);

  doc.setFontSize(14);
  doc.text('Diagnóstico Geral', x_position(), y_position());

  doc.setFontSize(12);
  doc.text(`Diagnóstico: ${diagnostico}`, x_position(), y_position());
  doc.text(`Solução: ${solucao}`, x_position(), y_position());

  y_line = y_position();
  doc.line(x_position(), y_line, 200, y_line);
  doc.setFontSize(14);
  doc.text('Explicações', x_position(), y_position());
  doc.setFontSize(12);

  if (explicacaoGeral){
    doc.text('Explicação Geral:', x_position(), y_position());
    doc.text(`${explicacaoGeral}`, x_position(), y_position());
    doc.text('Explicação Geral (Não):', x_position(), y_position());
    doc.text(`${explicacaoGeralNao}`, x_position(), y_position());
  }

  doc.text('Como chegou-se ao diagnóstico?', x_position(), y_position());
  //doc.text(como, 10, y_position());

//  doc.setFontSize(10);
if (como.isArray){

} else {
  if (como instanceof String){
    const como_array = lineBreaker(como);

    como_array.forEach((line) => {
      doc.text(line, x_position(1), y_position());
    });
  }
}

  doc.setFontSize(12);
  doc.text('Porquê estas evidências?', x_position(), y_position());

  //doc.setFontSize(10);
  if (evidencias && evidencias.length > 0) {
    evidencias.forEach((evidencia, index) => {
      doc.text(`${index + 1}. ${evidencia.fact}`, x_position(1), y_position());
    });
  } else {
    doc.text('Nenhuma evidência encontrada.', x_position(1), y_position());
  }

  doc.setFontSize(12);

  y_line = y_position();
  doc.line(x_position(), y_line, 200, y_line);
  doc.setFontSize(14);
  doc.text('Regras acionadas', x_position(), y_position());
  doc.setFontSize(12);
  if (triggeredRules && triggeredRules.length > 0) {
    triggeredRules.forEach((regra, index) => {
      let y = y_position();
      doc.text(`${index + 1}. ${regra}, ${y}`, x_position(1), y);
    });
  } else {
    doc.text('Nenhuma regra acionada.', x_position(1), y_position());
  }

  const pdfData = doc.output('bloburl');
  window.open(pdfData, '_blank');
};