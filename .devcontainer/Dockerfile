FROM rocker/r-ver:devel

RUN apt-get update && apt-get -y upgrade && apt-get -y install \
    wget \
    curl \
    git \
    pandoc \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
 

RUN Rscript -e "install.packages(c('languageserver', 'devtools'))"
