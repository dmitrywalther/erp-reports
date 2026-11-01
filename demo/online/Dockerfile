# Standalone public demo. PostgreSQL is private and ephemeral; only Shiny is exposed.
FROM rocker/r-ver:4.0.4 AS dependencies

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=ru_RU.UTF-8 LC_ALL=ru_RU.UTF-8 TZ=UTC ERP_REPORTS_DEMO=true \
    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1

# Prevent the package installer from starting a database during image build.
RUN printf '#!/bin/sh\nexit 101\n' > /usr/sbin/policy-rc.d \
    && chmod +x /usr/sbin/policy-rc.d \
    && sed -i 's|http://archive.ubuntu.com|https://archive.ubuntu.com|g; s|http://security.ubuntu.com|https://security.ubuntu.com|g' /etc/apt/sources.list \
    && apt-get -o Acquire::Retries=3 update \
    && apt-get -o Acquire::Retries=3 install --no-install-recommends -y \
       postgresql postgresql-client tini curl ca-certificates locales \
       libpq-dev libssl-dev libcurl4-openssl-dev libxml2-dev libsodium-dev \
       libudunits2-dev libproj-dev libgdal-dev libgeos-dev libcairo2-dev \
       libharfbuzz-dev libfribidi-dev libfontconfig1-dev libfreetype6-dev \
       libpng-dev libtiff5-dev libjpeg-dev libv8-dev \
    && sed -i 's/^# *ru_RU.UTF-8/ru_RU.UTF-8/' /etc/locale.gen && locale-gen \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY demo/install_packages.R demo/install_packages.R
RUN Rscript --vanilla demo/install_packages.R /usr/local/lib/R/site-library
# The archived igraph binary used by highcharter links to GLPK at runtime.
RUN apt-get -o Acquire::Retries=3 update \
    && apt-get -o Acquire::Retries=3 install --no-install-recommends -y libglpk40 \
    && rm -rf /var/lib/apt/lists/*

FROM dependencies AS runtime
RUN useradd --create-home --uid 1000 demo
COPY global.R helpers.R server.R ui.R production_calendar.R report_utils.R config.yml version ./
COPY ui/ ui/
COPY www/my_style.css www/my_style.css
COPY demo/ demo/
RUN chmod -R a=rX /app && chmod a+x demo/online/entrypoint.sh
USER demo
RUN Rscript --vanilla -e "source('global.R', encoding='UTF-8'); stopifnot(requireNamespace('V8', quietly=TRUE))"
ENV PORT=10000
EXPOSE 10000
ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["/app/demo/online/entrypoint.sh"]
