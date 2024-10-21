import * as Icons from '@mui/icons-material'; // Import all icons
import { Box, SvgIcon, Typography } from '@mui/material'; // Import SvgIcon for rendering icons
import React from 'react';

const TituloLinha = ({ title, lineColor = 'blue', icon, position = '0px'}) => {
  const IconComponent = icon ? Icons[icon] : null; // Get the icon component based on the icon prop

  return (
    <Box
      sx={{
        display: 'flex',
        alignItems: 'flex-end',
        paddingLeft: '5px',
        paddingBottom: '20px',
        marginLeft: position,
      }}
    >
      {/* Render icon */}
      {IconComponent && (
        <SvgIcon component={IconComponent} sx={{ paddingLeft: '0px', marginRight: '5px', fontSize: '2rem', color: 'inherit' }} />
      )}
      {/* <Box
        sx={{
          width: '10px', // Set width to 10px for the left line
          height: '1px', // Height of the line
          background: `${lineColor}`, // Gradient background
          marginBottom: '10px',
        }}
      /> */}
      <Typography variant="h6" sx={{ margin: 0, paddingRight: '5px' }}>
        {title}
      </Typography>
      <Box
        sx={{
          flex: 1,
          height: '1px', // Height of the line
          background: `linear-gradient(to right, ${lineColor}, transparent)`, // Gradient background
          marginBottom: '10px',
        }}
      />
    </Box>
  );
};

// const styles = {
//   container: {
//     display: 'flex',
//     alignItems: 'flex-end', // Align items at the bottom (base of the text)
//     paddingLeft: '0px',   // Add padding to the left
//     paddingBottom: '20px'
//   },
//   title: {
//     margin: 0,            // Remove default margin
//     paddingRight: '10px', // Space between title and line
//   },
//   line: {
//     flex: 1,              // Allow the line to grow and fill the remaining space
//     height: '1px',        // Height of the line
//     marginBottom: '10px',
//   },
//   icon: {
//     marginRight: '10px',  // Space between icon and title
//     fontSize: '2rem',      // Size of the icon
//     color: 'inherit',      // Inherit color from parent
//   },
// };

export default TituloLinha;
