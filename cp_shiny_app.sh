#!/bin/bash

# Copy shiny app into specified target directory
# The directory you specify would be something like OASIS-1-Results/results/gray

set -e

cp Scripts/Shiny/app.R $1
cp Scripts/ShinyVtkScripts/render.js $1
cp Atlas/sri24/labels/atlas.Rdata $1
cp Scripts/Shiny/shiny-help.md $1
