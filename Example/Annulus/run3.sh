IMAGES_PATH="./images3"
VAR_TABLE="./data3.csv"
RESULTS_PATH="Results3"
if [ ! -d "$IMAGES_PATH" ]; then
  mkdir $IMAGES_PATH
fi

#Create ellipse images
Rscript setup.discs3.R $IMAGES_PATH $VAR_TABLE

Rscript ../../Scripts/run.utm.barycenter.R $IMAGES_PATH $VAR_TABLE --working.folder $RESULTS_PATH

cp ../../Scripts/Shiny/app.R $RESULTS_PATH
cp ../../Scripts/Shiny/shiny-help.md $RESULTS_PATH
cp ../../Scripts/ShinyVtkScripts/render.js $RESULTS_PATH
