# Dockerfile.base image
FROM rocker/verse:4.5.1

# 1) remotes & renv pinned
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')" && \
    R -e "remotes::install_version('remotes', version='2.4.2', repos='https://cloud.r-project.org')" && \
    R -e "remotes::install_version('renv',    version='1.1.4', repos='https://cloud.r-project.org')"

# 2) restore all your locked packages
COPY renv.lock .Rprofile activate.R ./
RUN R -e "options(renv.consent=TRUE, Ncpus=parallel::detectCores()); renv::restore()"

# nothing else—this image is just "R+all your renv.lock packages"