IMAGES_PATH="./images2"
VAR_TABLE="./data2.csv"
if [ ! -d "$IMAGES_PATH" ]; then
  mkdir $IMAGES_PATH
fi

#Create ellipse images
Rscript setup.discs2.R $IMAGES_PATH $VAR_TABLE

Rscript ../../Scripts/run.utm.barycenter.R $IMAGES_PATH $VAR_TABLE --working.folder "Results2"

cp ../../Scripts/Shiny/app.R Results2
cp ../../Scripts/Shiny/shiny-help.md Results2
cp ../../Scripts/ShinyVtkScripts/render.js Results2
