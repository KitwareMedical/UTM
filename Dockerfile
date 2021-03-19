FROM utm-base 


RUN mkdir UTM
COPY ./Scripts/ UTM/Scripts

#insatll local packages
RUN /bin/bash -c "source /UTM-Packages/install-local.sh"

RUN mkdir App 
WORKDIR UTM/App


ENTRYPOINT ["Rscript",  "../Scripts/run.utm.barycenter.R"] 

