IMAGES_PATH="./images2"
VAR_TABLE="./data2.csv"
RESULTS_PATH="Results2"
if [ ! -d "$IMAGES_PATH" ]; then
  mkdir $IMAGES_PATH
fi

#Create ellipse images
Rscript setup.discs2.R $IMAGES_PATH $VAR_TABLE

Rscript ../../Scripts/run.utm.barycenter.R $IMAGES_PATH $VAR_TABLE --working.folder $RESULTS_PATH

cp ../../Scripts/Shiny/app.R $RESULTS_PATH
cp ../../Scripts/Shiny/shiny-help.md $RESULTS_PATH
cp ../../Scripts/ShinyVtkScripts/render.js $RESULTS_PATH
