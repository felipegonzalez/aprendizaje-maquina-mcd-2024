FROM rocker/rstudio:4.4.1

ENV CRAN=https://packagemanager.posit.co/cran/__linux__/jammy/2024-08-07
ENV LANG=en_US.UTF-8

ENV WORKON_HOME /opt/virtualenvs
ENV PYTHON_VENV_PATH $WORKON_HOME/am2024_env

COPY scripts /rocker_scripts

RUN /rocker_scripts/install_tidyverse.sh

ENV CTAN_REPO=https://www.texlive.info/tlnet-archive/2024/07/09/tlnet
ENV PATH=$PATH:/usr/local/texlive/bin/linux

RUN /rocker_scripts/install_verse.sh

RUN apt-get update && apt-get install -y pngquant libhdf5-dev

RUN apt-get update && apt-get install -y --no-install-recommends \
        python3.11-dev \
        python3.11-venv &&\
    rm -rf /var/lib/apt/lists/*

## Prepara environment de python
RUN python3.11 -m venv ${PYTHON_VENV_PATH}
RUN chown -R rstudio:rstudio ${WORKON_HOME}
ENV PATH ${PYTHON_VENV_PATH}/bin:${PATH}
RUN echo "PATH=${PATH}" >> /usr/local/lib/R/etc/Renviron && \
    echo "WORKON_HOME=${WORKON_HOME}" >> /usr/local/lib/R/etc/Renviron && \
    echo "RETICULATE_PYTHON_ENV=${PYTHON_VENV_PATH}" >> /usr/local/lib/R/etc/Renviron

## Because reticulate hardwires these PATHs
RUN ln -s ${PYTHON_VENV_PATH}/bin/pip /usr/local/bin/pip && \
    ln -s ${PYTHON_VENV_PATH}/bin/virtualenv /usr/local/bin/virtualenv
RUN chmod -R a+x ${PYTHON_VENV_PATH}

RUN .${PYTHON_VENV_PATH}/bin/activate && \
    pip install wheel && \
    pip install radian Pillow requests scipy pandas\
    tensorflow==2.17.0 keras==3.4.1
  
RUN install2.r --error Rcpp
RUN install2.r --error tidymodels embed textrecipes workflowsets bonsai iml pdp
RUN install2.r --error \
    abind splines2 kableExtra gt\
    glmnet quantreg xgboost\
    ranger baguette rpart.plot\
    doParallel doFuture \
    kernlab kknn\
    tsne irlba tokenizers discrim

RUN apt-get update && apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev

RUN install2.r --error sf ggrepel gganimate patchwork imager

RUN install2.r reticulate tensorflow keras3 \
    abind RcppRoll
RUN install2.r gifski languageserver
RUN install2.r quantregForest


