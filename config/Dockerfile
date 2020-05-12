FROM rocker/rstudio:3.6.3

RUN apt-get update && apt-get install -y \
    curl \
    gnupg \
    apt-transport-https \
    ca-certificates \
    ssh-client \
    libjpeg-dev \
    libpoppler-cpp-dev \
    libz-dev
RUN echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
RUN curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -
RUN sudo apt-get update && sudo apt-get install -y google-cloud-sdk

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    pdftools \ 
## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
    
COPY files/gs_loader.R /home/script.R
