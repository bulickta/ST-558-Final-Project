FROM rocker/r-ver:4.4.0
WORKDIR /API
RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('caret')"
RUN R -e "install.packages('plumber')"
COPY . /API/.
EXPOSE 8000
ENTRYPOINT ["R", "-e", \
"pr <- plumber::plumb('/API/myAPI.R'); pr$run(host='0.0.0.0', port=8000)"]

