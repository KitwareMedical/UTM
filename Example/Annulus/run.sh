IMAGES_PATH="./images"
VAR_TABLE="./data.csv"
if [ ! -d "$IMAGES_PATH" ]; then
  mkdir $IMAGES_PATH
fi

#Create ellipse images
Rscript setup.discs.R $IMAGES_PATH $VAR_TABLE

Rscript ../../Scripts/run.utm.barycenter.R $IMAGES_PATH $VAR_TABLE --working.folder "Results"

cp ../../Scripts/Shiny/app.R Results
cp ../../Scripts/Shiny/shiny-help.md Results
cp ../../Scripts/ShinyVtkScripts/render.js Results
