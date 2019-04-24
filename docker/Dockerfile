FROM rocker/rstudio

# Install some missing dependencies
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  zlib1g-dev \
  libxml2-dev \
  cmake

# Get code from github
RUN git clone https://github.com/fabianmax/cv-simulation.git /home/rstudio/cv_simulation

# Install r packages
RUN Rscript /home/rstudio/cv_simulation/libs_install.R

# Install lightgbm
RUN git clone --recursive https://github.com/Microsoft/LightGBM /tmp/LightGBM && \
    cd /tmp/LightGBM/ && \
    Rscript build_r.R

# Give rights to rstudio user
RUN chown -R rstudio: /home/rstudio/cv_simulation && \
    chmod -R u+w /home/rstudio/cv_simulation

EXPOSE 8787

CMD ["/init"]
