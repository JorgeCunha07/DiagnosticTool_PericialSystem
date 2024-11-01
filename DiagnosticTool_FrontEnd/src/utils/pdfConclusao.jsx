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
  selectedFalha, // porqueNao diagnostico alternativo
  falhaDetalhes, // detalhes do porque nao
  responseText, // detalhes do porque
  activeButtonIndex, // index to porque
) => {
  const doc = new jsPDF();
  const altura_pagina = doc.internal.pageSize.height;  // 298 eh o tamanho A4
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

  const y_position = (function () {
    let y = margem_topo_pagina;
    
    return (posicao_diferente) => {
      if (y >= (altura_pagina - margem_baixo_pagina)) {
        paginaAtual++;
        doc.addPage();
        addFooter();
        y = margem_topo_pagina;
      }
      y += (typeof posicao_diferente === 'number') ? posicao_diferente : posicao_entrelinhas;
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

  const addWrappedText = (text, fontsize = 12, identation = 0) => {
    //const wrappedText = doc.splitTextToSize(text, lineWidth - margem_esquerda - (identation * margem_esquerda));
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

  //addWrappedText("Lorem ipsum dolor sit amet, consectetur adipiscing elit. In ac semper neque. Cras vel metus bibendum mi tempus aliquam. In nec metus nunc. Donec sed tristique eros. Etiam in enim non odio aliquam mollis eget nec nibh. Donec feugiat nibh vitae nulla vulputate efficitur. Sed commodo, enim id porttitor condimentum, arcu ex placerat mi, sed volutpat est odio sed nulla. Aenean tincidunt tellus in elit malesuada consectetur a at purus. Nam rutrum, sem sed tincidunt rhoncus, leo ligula euismod eros, ac volutpat mi libero ut felis. Nam ornare ultricies urna, sit amet rhoncus lorem pulvinar bibendum. Donec consectetur augue eu sem sagittis rutrum. Nulla non libero enim. Cras rhoncus urna in arcu malesuada, et sollicitudin tortor pretium. Duis nec neque et massa lobortis ullamcorper. In tristique justo vel egestas iaculis. Aenean ac nisl finibus, vulputate ipsum congue, malesuada dolor.", fontsize_body, 2);

  if(carro) {
    drawHorizontalLine();

    addWrappedText('Veículo Analisado', fontsize_h2);
    addWrappedText(`Marca: ${carro.marca?.nome || 'N/A'}`);
    addWrappedText(`Modelo: ${carro.modelo?.nome || 'N/A'}`);
    addWrappedText(`Motor: ${carro.motor?.nome || 'N/A'}`);
    addWrappedText('Níveis ideais dos componentes para este veículo');

    doc.setFontSize(10);
    if (carro.componentes?.length > 0) {
      const column1X = x_position(1); // X para coluna esquerda
      const column2X = largura_pagina/2; // X para coluna direita
      let yPosition = y_position();

      carro.componentes.forEach((componente, index) => {
        doc.setFontSize(fontsize_body);
        const text = `${componente.nome}: ${componente.valorMinimoIdeal} - ${componente.valorMaximoIdeal} ${componente.unidade}`;
        if (index % 2 === 0) {
          doc.text(text, column1X, yPosition);
        } else {
          doc.text(text, column2X, yPosition);
          yPosition = y_position();
        }
      });

      // if (carro.componentes.length % 2 !== 0) {
      //   //yPosition = y_position();
      // }

    } else {
      addWrappedText('Nenhum componente encontrado.', fontsize_body, 1);
    }
}

if (diagnostico && solucao){
  drawHorizontalLine();
  addWrappedText('Diagnóstico Geral', fontsize_h2);

  addWrappedText(`Diagnóstico: ${diagnostico}`);
  addWrappedText(`Solução: ${solucao}`);
}

if (explicacaoGeral){
  drawHorizontalLine();
  addWrappedText('Explicações', fontsize_h2);

  addWrappedText('Explicação Geral:');
  addWrappedText(`${explicacaoGeral || 'N/A'}`);
  addWrappedText('Explicação Geral (Não):');
  addWrappedText(`${explicacaoGeralNao || 'N/A'}`);
}

  if (como) {
    drawHorizontalLine();
    addWrappedText('Como chegou-se ao diagnóstico?', fontsize_h2);

    const como_array = como.split('\n');
    como_array.forEach((line) => {
      addWrappedText(line, fontsize_body, 1);
    });
  } else {
    addWrappedText('Nenhuma informação encontrada.', fontsize_body, 1);
  }

  // Porque
  if (evidencias) {
    drawHorizontalLine();
    addWrappedText('Porquê esta evidência?', fontsize_h2);

    if (evidencias.length > 0) {
      evidencias.forEach((evidencia, index) => {

      // para mostrar todas perguntas
      //addWrappedText(`${index}. ${evidencia.fact}`, fontsize_body, 1);
        
        if (activeButtonIndex === index && responseText) {
          // apenas mostra a pergunta que há resposta
          addWrappedText(`${index + 1}. ${evidencia.fact}`, fontsize_body, 1);

          addWrappedText(`Resposta: ${responseText}`, fontsize_body, 1);
        }
      });
    } else {
      addWrappedText('Nenhuma evidência encontrada.', fontsize_body, 1);
    }
  }

  // PorqueNao
  if(falhaDetalhes){
    if (falhaDetalhes.length > 0 && selectedFalha) {
      drawHorizontalLine();
      addWrappedText('Porque não outro diagnóstico?', fontsize_h2);

      addWrappedText(`Diagnóstico alternativo: ${selectedFalha}`, fontsize_body);
      addWrappedText(`Para este diagnóstico ocorrer, as regras abaixo deveriam ter disparado:`, fontsize_body, 1);
      falhaDetalhes.forEach((detalhe, index) => {
        addWrappedText(`${index + 1}. ${detalhe}`, fontsize_body, 1);
      });
    }
  }

  if (triggeredRules){
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
