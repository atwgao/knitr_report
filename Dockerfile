# Install R version 3.5
FROM r-base:3.5.0

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev

RUN wget -qO- "https://yihui.name/gh/tinytex/tools/install-unx.sh" | sh

# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "install.packages(c('shiny', 'shinydashboard','ppcor', 'dplyr','rmarkdown','DT'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('htmlwidgets', 'httpuv'), dependencies = TRUE)"
RUN R -e "install.packages(c('shinyalert','Hmisc', 'plotly'), dependencies = TRUE)"
RUN R -e "install.packages(c(kableExtra), dependencies = TRUE)"
# copy the app to the image
RUN mkdir /root/knitr_report
COPY knitr_report /root/knitr_report

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/knitr_report')"]