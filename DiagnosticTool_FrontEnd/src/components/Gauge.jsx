import { scaleLinear } from "d3-scale";
import { arc } from "d3-shape";
import React from "react";

const Gauge = ({
  value = null,
  min = 0,
  max = 8.5,
  label,
  units,
  minIdeal = 4.8,
  maxIdeal = 8.5,
}) => {
  let displayedValue = null;;
  displayedValue = minIdeal === maxIdeal ? minIdeal : null

  const percentScale = scaleLinear()
    .domain([min, max])
    .range([0, 1])
    
  const percentIdealMin = percentScale(minIdeal)
  const percentIdealMax = percentScale(maxIdeal)
  const percentValue = percentScale(displayedValue || value)

  const angleScale = scaleLinear()
    .domain([0, 1])
    .range([-Math.PI / 2, Math.PI / 2])
    .clamp(true)

  const angleValue = angleScale(percentValue)
  const angleMinIdeal = angleScale(percentIdealMin)
  const angleMaxIdeal = angleScale(percentIdealMax)


  // Arco: fundo
  const bgArc = arc()
    .innerRadius(0.65)
    .outerRadius(1)
    .startAngle(-Math.PI / 2)
    .endAngle(Math.PI / 2)
    .cornerRadius(1)
    ()

  // Arco:  min-minIdeal, abaixo do minimo ideal
  // const filledArcMinToIdeal = arc()
  //   .innerRadius(0.65)
  //   .outerRadius(1)
  //   .startAngle(-Math.PI / 2)
  //   .endAngle(angleMinIdeal)
  //   .cornerRadius(1)
  //   ()

  // Arco: minIdeal-maxIdeal
  const filledArcIdealRange = arc()
    .innerRadius(0.65)
    .outerRadius(1)
    .startAngle(angleMinIdeal)
    .endAngle(angleMaxIdeal)
    .cornerRadius(1)
    ()

  // Arco: maxIdeal-max, em excesso ao maximo ideal
  // const filledArcIdealToMax = arc()
  //   .innerRadius(0.65)
  //   .outerRadius(1)
  //   .startAngle(angleMaxIdeal)
  //   .endAngle(Math.PI / 2)
  //   .cornerRadius(1)
  //   ()

  const markerLocation = getCoordsOnArc(angleValue, 1 - ((1 - 0.65) / 2))

  return (
    <div style={{ textAlign: "center" }}>
      <svg style={{ overflow: "visible" }} width="9em" viewBox="-1 -1 2 1">

      <defs>
        <linearGradient id="Gauge__gradientGreen" gradientUnits="userSpaceOnUse" x1="0" y1="0" x2="1" y2="0">
          <stop offset="0%" stopColor="#00b300" stopOpacity="0.5" />
          <stop offset="100%" stopColor="#00b300" stopOpacity="1" />
        </linearGradient>
      </defs>

        <path d={bgArc} fill="#dbdbe7" />

        {/* Arco: min-minIdeal */}
        {/* <path d={filledArcMinToIdeal} fill="#ffcdd2" /> */}

        {/* Arco: MinIdeal-MaxIdeal */}
        <path d={filledArcIdealRange} fill="url(#Gauge__gradientGreen)" />

        {/* Arco: maxIdeal-max */}
        {/* <path d={filledArcIdealToMax} fill="#ffe0b2" /> */}

        {/* Linha no meio */}
        {/* <line y1="-1" y2="-0.65" stroke="white" strokeWidth="0.027" /> */}

        {(value || displayedValue) && (
          <>
            <circle
              cx={markerLocation[0]}
              cy={markerLocation[1]}
              r="0.13"
              fill="#4834d4"
              //stroke="white"
              strokeWidth="0.01" />

            {/* Agulha */}
            <path
              d="M0.136364 -0.169852C0.151814 -0.211538 0.219156 -0.208532 0.241071 -0.169852C0.375 0.198066 0.375 0.254949 0.375 0.254949C0.375 0.352787 0.292208 0.430148 0.1875 0.430148C0.0852272 0.430148 -8.55326e-09 0.352787 0 0.254949C0 0.254949 0 0.198066 0.136364 -0.169852ZM0.1875 0.311832C0.221591 0.311832 0.248377 0.286803 0.248377 0.254949C0.248377 0.223095 0.221591 0.198066 0.1875 0.198066C0.153409 0.198066 0.126623 0.223095 0.126623 0.254949C0.126623 0.286803 0.155844 0.311832 0.1875 0.311832Z"
              transform={`rotate(${angleValue * (180 / Math.PI)}) translate(-0.2, -0.33)`}
              fill="#6A6A85"
            />

          </>
        )}
      </svg>
      
      {!!units && (
        <div style={{ position: "relative", top: "10px", left: "145px", textAlign: "left", color: "#8b8ba7", lineHeight: "1.3em", fontWeight: "300" }}>
          {units}
        </div>
      )}

      <div style={{ position: "relative", top: "-18px", fontSize: "1.35em", fontWeight: "900" }}>
        {value ? value : (displayedValue ? displayedValue : `${minIdeal} a ${maxIdeal}`)}
        {/* {displayedValue ? displayedValue : `${minIdeal} a ${maxIdeal}`} */}
      </div>

      {!!label && (
        <div style={{ position: "relative", top: "-25px", color: "#8b8ba7", marginTop: "0.6em", fontSize: "1.02em"}}>
          <b>{label}</b>
          {/* {!!units && (` (${units})`)} */}
        </div>
      )}
    </div>
  )
}

const getCoordsOnArc = (angle, offset = 10) => [
  Math.cos(angle - Math.PI / 2) * offset,
  Math.sin(angle - Math.PI / 2) * offset,
]

export default Gauge