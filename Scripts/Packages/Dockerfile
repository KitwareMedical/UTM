FROM rocker/r-ver


RUN apt-get update 

RUN apt-get install -y --no-install-recommends texlive-latex-base
#RUN apt-get install -y --no-install-recommends texlive-fonts-recommended
#RUN apt-get install -y --no-install-recommends texlive-fonts-extra
RUN apt-get install -y --no-install-recommends texlive-latex-extra

RUN apt-get install -y --no-install-recommends libpng-dev

RUN apt-get -y install -y --no-install-recommends parallel
RUN printf "will cite\n" | parallel --citation

RUN apt-get install -y --no-install-recommends xorg 
RUN apt-get install -y --no-install-recommends libx11-dev
#RUN printf "Y\n" | apt-get install r-cran-rgl
RUN apt-get install -y --no-install-recommends libgl1-mesa-dev
RUN apt-get install -y --no-install-recommends libglu1-mesa-dev
RUN apt-get install -y --no-install-recommends freeglut3-dev


RUN mkdir UTM-packages
COPY ./ UTM-Packages

RUN /bin/bash -c "source /UTM-Packages/install-base.sh"


CMD ["R"]
