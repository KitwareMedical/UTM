install.packages( c("optparse",
                    "png",
                    "shape",
                    "data.table",
                    "lmvar",
                    "stringr",
                    "foreach",
                    "doParallel",
                    "RColorBrewer",
                    "data.table",
                    "pracma",
                    "glmnet",
                    "shiny",
                    "mmand"
                    "shinyBS",
                    "broom",
                    "markdown",
                    "shiny",
                    "corrplot",
                    "shinyWidgets",
                    "shinythemes",
                    "shinyBS",
                    "yaml",
                    "DT"
                    ),
                   repos='http://cran.us.r-project.org'
                 )


mydeps <- c( "devtools", "Rcpp", "RcppEigen", "magrittr", "rsvd", "magic", "psych" )
install.packages( pkgs = mydeps, dependencies = TRUE )
library( devtools )
# install_github("stnava/cmaker") # if you do not have cmake
install_github("stnava/ANTsR")

install_github("samuelgerber/vtkwidgets")

