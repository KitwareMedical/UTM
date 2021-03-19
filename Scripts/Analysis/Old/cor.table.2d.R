preamble.string<- "
\\documentclass{article} 
\\usepackage{graphicx} 
\\usepackage{array,multirow} 
\\graphicspath{{./}} 
\\DeclareGraphicsExtensions{.png} 

\\begin{document}

"

table.string <- 
"
\\begingroup 
\\renewcommand{\\arraystretch}{0} 
\\setlength{\\tabcolsep}{0pt} 
\\begin{figure}[h!] 
\\centering 
\\begin{tabular}{l|ccc} 
VBM & UTM-T & UTM-A \\\\
\\includegraphics[width=0.3\\linewidth]{cor-PLACEHOLDER-intensity} & 
\\includegraphics[width=0.3\\linewidth]{cor-PLACEHOLDER-transport} & 
\\includegraphics[width=0.3\\linewidth]{cor-PLACEHOLDER-allocation} \\\\
\\includegraphics[width=0.3\\linewidth]{cor-PLACEHOLDER-t-intensity} & 
\\includegraphics[width=0.3\\linewidth]{cor-PLACEHOLDER-t-transport} & 
\\includegraphics[width=0.3\\linewidth]{cor-PLACEHOLDER-t-allocation} \\\\ 
\\includegraphics[width=0.3\\linewidth]{cor-PLACEHOLDER-p-intensity} & 
\\includegraphics[width=0.3\\linewidth]{cor-PLACEHOLDER-p-transport} & 
\\includegraphics[width=0.3\\linewidth]{cor-PLACEHOLDER-p-allocation} &  
\\end{tabular} 
\\caption{\\label{fig:cor-PLACEHOLDER} 
Correlation of PLACEHOLDER  with voxel intensity, UTM allocation and UTM transport costs. 
Correlations are only shown at locations permutation tested p--value less than 
0.05. The background image are average white and gray matter segmentations.  
\\vspace{-7mm} 
}  
\\end{figure}  
\\endgroup   
"


library(stringr)

args <- commandArgs( trailingOnly=TRUE)

variables.path <- args[1]
save.path <- args[2]
filename <- args[3]

tex.string <- preamble.string

load(variables.path)
for(v in variables ){
    tmp <- str_replace_all(table.string, "PLACEHOLDER", v$name)
    tex.string <-paste(tex.string, tmp)
}
tex.string <- paste( tex.string, "\\end{document} ")



writeLines( tex.string, sprintf("%s/%s", save.path, filename) )


