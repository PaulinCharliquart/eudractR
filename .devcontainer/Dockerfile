ARG VARIANT="4.2.2"

FROM rocker/r-ver:${VARIANT}

RUN apt-get update && apt-get -y upgrade
RUN apt-get -y install wget \
                       curl \
                       gdebi-core \
                       git \
                       pandoc \
                       python3-pip \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
 

RUN Rscript -e "install.packages(c('languageserver', 'devtools'))"
