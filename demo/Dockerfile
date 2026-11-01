# Образ для демо-стенда: R 4.0.4 + пакеты из снимка Posit Package Manager.
FROM rocker/r-ver:4.0.4

RUN apt-get update && apt-get install --no-install-recommends -y \
    libpq-dev libssl-dev libcurl4-openssl-dev libxml2-dev libsodium-dev \
    libudunits2-dev libproj-dev libgdal-dev libgeos-dev libcairo2-dev \
    locales && rm -rf /var/lib/apt/lists/* \
    && sed -i 's/^# *ru_RU.UTF-8/ru_RU.UTF-8/' /etc/locale.gen && locale-gen

ENV LANG=ru_RU.UTF-8 LC_ALL=ru_RU.UTF-8 TZ=Europe/Moscow ERP_REPORTS_DEMO=true

WORKDIR /app
COPY demo/install_packages.R demo/install_packages.R
RUN Rscript demo/install_packages.R /usr/local/lib/R/site-library

COPY . .
EXPOSE 3838
