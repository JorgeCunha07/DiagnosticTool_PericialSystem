import { format } from "d3-format"
import { scaleLinear } from "d3-scale"
import { arc } from "d3-shape"
import React from "react"

const Gauge = ({
  value=6.5,
  min=0,
  max=8.5,
  label,
  units,
  max_ideal=8.5,
}) => {
  const backgroundArc = arc()
    .innerRadius(0.65)
    .outerRadius(1)
    .startAngle(-Math.PI / 2)
    .endAngle(Math.PI / 2)
    .cornerRadius(1)
    ()

  const percentScale = scaleLinear()
    .domain([min, max])
    .range([0, 1])
    
  const percentScale_red = scaleLinear()
    .domain([value, max_ideal])
    .range([0, 1])

  const percent = percentScale(value)
  const percent_red = percentScale_red(max_ideal)

  const angleScale = scaleLinear()
    .domain([0, 1])
    .range([-Math.PI / 2, Math.PI / 2])
    .clamp(true)

  const angleScale_red = scaleLinear()
    .domain([0, 1])
    .range([(-Math.PI / 2), Math.PI / 2])
    .clamp(true)

  const angle = angleScale(percent)

  const angle_red = angleScale_red(percent_red)

  const filledArc1 = arc()
    .innerRadius(0.65)
    .outerRadius(1)
    .startAngle(-Math.PI / 2)
    .endAngle(angle)
    .cornerRadius(1)
    ()

    const filledArc2 = arc()
    .innerRadius(0.65)
    .outerRadius(1)
    .startAngle((-Math.PI / 2))
    .endAngle(angle_red)
    .cornerRadius(1)
    ()

  const colorScale = scaleLinear()
    .domain([0, 1])
    .range(["#dbdbe7", "#4834d4"])

  const colorScale_red = scaleLinear()
    .domain([0, 1])
    .range(["#d80000", "#d80000"])

    const gradientSteps_red = colorScale_red.ticks(10)
    .map(value => colorScale(value))

  const gradientSteps = colorScale.ticks(10)
    .map(value => colorScale(value))

  const markerLocation = getCoordsOnArc(
    angle,
    1 - ((1 - 0.65) / 2),
  )

  return (
    <div
      style={{
        textAlign: "center",
      }}>
      <svg style={{overflow: "visible"}}
        width="9em"
        viewBox={[
          -1, -1,
          2, 1,
        ].join(" ")}>
        <defs>
          <linearGradient
            id="Gauge__gradient1"
            gradientUnits="userSpaceOnUse"
            x1="-1"
            x2="1"
            y2="0">
            {gradientSteps.map((color, index) => (
              <stop
                key={color}
                stopColor={color}
                offset={`${
                  index
                  / (gradientSteps.length - 1)
                }`}
              />
            ))}
          </linearGradient>
          {/* <linearGradient
            id="Gauge__gradient_red"
            gradientUnits="userSpaceOnUse"
            x1="-1"
            x2="1"
            y2="0">
            {gradientSteps_red.map((color, index) => (
              <stop
                key="{color}"
                stopColor={color}
                offset={`${
                  index
                  / (gradientSteps_red.length - 1)
                }`}
              />
            ))}
          </linearGradient> */}
        </defs>
        <path
          d={backgroundArc}
          fill="#dbdbe7"
        />
        {/* Preenchimento gradiente */}
        <path
          d={filledArc1}
          fill="url(#Gauge__gradient1)"
        />
        {/* Preenchimento gradiente 2 */}
        <path
          d={filledArc2}
          fill="url(#Gauge__gradient_red)"
        />
        {/* Linha branca no meio */}
        <line
          y1="-1"
          y2="-0.65"
          stroke="white"
          strokeWidth="0.027"
        />
        {/* <circle
          cx={markerLocation[0]}
          cy={markerLocation[1]}
          r="0.2"
          stroke="#2c3e50" // #2700ff
          strokeWidth="0.01"
          fill={colorScale(percent)}
        /> */}
        {/* <path
          d="M0.136364 0.0290102C0.158279 -0.0096701 0.219156 -0.00967009 0.241071 0.0290102C0.297078 0.120023 0.375 0.263367 0.375 0.324801C0.375 0.422639 0.292208 0.5 0.1875 0.5C0.0852272 0.5 -1.8346e-08 0.422639 -9.79274e-09 0.324801C0.00243506 0.263367 0.0803571 0.120023 0.136364 0.0290102ZM0.1875 0.381684C0.221591 0.381684 0.248377 0.356655 0.248377 0.324801C0.248377 0.292947 0.221591 0.267918 0.1875 0.267918C0.153409 0.267918 0.126623 0.292947 0.126623 0.324801C0.126623 0.356655 0.155844 0.381684 0.1875 0.381684Z"
          transform={`rotate(${
            angle * (180 / Math.PI)
          }) translate(-0.2, -0.33)`}
          fill="#6a6a85"
        /> */}
      </svg>
      <div style={{
        marginTop: "0.4em",
        fontSize: "3em",
        lineHeight: "1em",
        fontWeight: "900",
        fontFeatureSettings: "'zero', 'tnum' 1",
      }}>
        { format(",")(value) }
      </div>
      {!!label && (
        <div style={{
          color: "#8b8ba7", // Cor da label
          marginTop: "0.6em",
          fontSize: "1.3em",
          lineHeight: "1.3em",
          fontWeight: "700",
        }}>
          { label }
        </div>
      )}
      {!!units && (
        <div style={{
          color: "#8b8ba7",
          lineHeight: "1.3em",
          fontWeight: "300",
        }}>
          { units }
        </div>
      )}
    </div>
  )
}
const getCoordsOnArc = (angle, offset=10) => [
  Math.cos(angle - (Math.PI / 2)) * offset,
  Math.sin(angle - (Math.PI / 2)) * offset,
]

export default Gauge;