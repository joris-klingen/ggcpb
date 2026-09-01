#!/bin/bash
cd "$(dirname "$0")"
Rscript run_import.R
echo
read -p "Press Enter to close this window: " dummy
