#!/bin/bash
cd "$(dirname "$0")"
Rscript generate_plot.R
echo
read -p "Press Enter to close this window: " dummy
