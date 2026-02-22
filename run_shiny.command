#!/bin/bash
cd "$(dirname "$0")/energy_macro_system" || exit 1
echo "Starting Energy–Macro Shiny Dashboard..."
echo "Browser will open at http://localhost:3838"
echo "Press Ctrl+C to stop the app."
echo ""
R -e "shiny::runApp(port = 3838, launch.browser = TRUE)"

