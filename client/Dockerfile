# client/Dockerfile
FROM myapp/r-base

WORKDIR /app

# copy only client code 
COPY client/R/       R/
COPY client/tests/   tests/

#ENTRYPOINT ["Rscript", "R/client_optimize.R"]
