FROM rocker/shiny-verse:4.0.4
WORKDIR /app
COPY . ./
RUN Rscript install.R
EXPOSE 80
CMD sh start.sh