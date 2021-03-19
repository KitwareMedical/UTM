preamble.string<- "
\\documentclass{article} 
\\usepackage{graphicx} 
\\usepackage{array,multirow} 
\\graphicspath{{./}} 
\\DeclareGraphicsExtensions{.png} 

\\begin{document}

"

table.string.3d <- 
"
\\section{ PLACEHOLDER }
\\begingroup 
\\renewcommand{\\arraystretch}{0} 
\\setlength{\\tabcolsep}{0pt} 
\\begin{figure}[h!] 
\\centering 
\\begin{tabular}{l|ccc} 
\\raisebox{12.5mm}{\\rotatebox[origin=c]{90}{VBM}}&           
\\includegraphics[angle=180,origin=c,width=0.3\\linewidth]{cor-axial-PLACEHOLDER-t-vbm} & 
\\reflectbox{\\includegraphics[width=0.3\\linewidth]{cor-coronal-PLACEHOLDER-t-vbm}} & 
\\includegraphics[width=0.3\\linewidth]{cor-sagittal-PLACEHOLDER-t-vbm} \\\\ \\hline \\hline 
\\raisebox{12.5mm}{\\rotatebox[origin=c]{90}{UTM-T}}&    
\\includegraphics[angle=180,origin=c,width=0.3\\linewidth]{cor-axial-PLACEHOLDER-t-transport} & 
\\reflectbox{\\includegraphics[width=0.3\\linewidth]{cor-coronal-PLACEHOLDER-t-transport}}& 
\\includegraphics[width=0.3\\linewidth]{cor-sagittal-PLACEHOLDER-t-transport} \\\\  
\\raisebox{12.5mm}{\\rotatebox[origin=c]{90}{\\raisebox{1.5mm}{UTM-A}}}&  
\\includegraphics[angle=180,origin=c,width=0.3\\linewidth]{cor-axial-PLACEHOLDER-t-allocation} &  
\\reflectbox{\\includegraphics[width=0.3\\linewidth]{cor-coronal-PLACEHOLDER-t-allocation}} & 
\\includegraphics[width=0.3\\linewidth]{cor-sagittal-PLACEHOLDER-t-allocation}  
\\end{tabular} 
\\caption{\\label{fig:cor-PLACEHOLDER} 
Correlation of PLACEHOLDER  with voxel vbm, UTM allocation and UTM transport costs. 
Correlations are only shown at locations permutation tested p--value less than 
0.05.   
\\vspace{-7mm} 
}  
\\end{figure}  
\\endgroup   
\\newpage
"


table.string.2d <- 
"
\\section{ PLACEHOLDER }
\\begingroup 
\\renewcommand{\\arraystretch}{0} 
\\setlength{\\tabcolsep}{0pt} 
\\begin{figure}[h!] 
\\centering 
\\begin{tabular}{l|ccccc} 
VBM & UTM-T & UTM-A & Conv & VBM-A\\\\
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-vbm} & 
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-transport} & 
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-allocation} &
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-convolutional} & 
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-vbmallocation} \\\\
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-t-vbm} & 
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-t-transport} & 
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-t-allocation} & 
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-t-convolutional}  &
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-t-vbmallocation}  \\\\ 
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-p-vbm} & 
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-p-transport} & 
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-p-allocation} &  
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-p-convolutional} &  
\\includegraphics[width=0.2\\linewidth]{cor-PLACEHOLDER-p-vbmallocation}   
\\end{tabular} 
\\caption{\\label{fig:cor-PLACEHOLDER} 
Correlation of PLACEHOLDER  with voxel vbm, UTM allocation and UTM transport costs. 
Correlations are only shown at locations permutation tested p--value less than 
0.05.  
\\vspace{-7mm} 
}  
\\end{figure}  
\\endgroup  
\\newpage
"



library(stringr, quietly=TRUE)

args <- commandArgs( trailingOnly=TRUE)

variables.path <- args[1]
save.path <- args[2]
filename <- args[3]

tex.string <- preamble.string

load(variables.path)
if( length(dims) == 2 ){
  table.string = table.string.2d
} else if( length(dims) == 3 ){
  table.string = table.string.3d
} else{
  q()
}

for(v in variables ){
    tmp <- str_replace_all(table.string, "PLACEHOLDER", v$name)
    tex.string <-paste(tex.string, tmp)
}
tex.string <- paste( tex.string, "\\end{document} ")



writeLines( tex.string, sprintf("%s/%s", save.path, filename) )


