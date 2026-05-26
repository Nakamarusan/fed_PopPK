# Dockerfile.app
# Unified runtime image for both client and server services.
FROM fedpoppk/base:0.1.0

WORKDIR /project

# 1) Copy renv configuration files first.
COPY renv.lock .Rprofile renv/activate.R ./

# 2) Disable sandboxing and restore the project library.
ENV R_PROFILE_USER=/project/.Rprofile \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE

RUN Rscript -e "options(renv.consent=TRUE, Ncpus=parallel::detectCores()); \
                renv::restore(prompt=FALSE)"

# 3) Copy application and shared code for all runtime modes.
COPY apps/ apps/
COPY R/ R/

# 4) Prepare mount targets.
RUN mkdir -p /project/configs/modes /project/data /project/artifacts /project/result
