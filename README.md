# Data and Code

Federated Estimation for Population Pharmacokinetic Models with Exact Equivalence to Pooled-Data Analysis

Yuta Nakamaru, Mizuki Uno, Yiran Song, Kanako So, Tomoko Kita, Fumiyoshi Yamashita

This repository contains the data, code, configurations, and analysis outputs for the Federated PopPK analysis reported in the manuscript.

## Repository Structure

- `data/`: generated datasets used in the analysis, including bootstrap datasets.
- `programs/`: R programs, resolved configuration files, Docker files, and entrypoints for rerunning the analysis.
- `results/`: disclosed centralized and federated analysis outputs.
- `figures_tables/`: manuscript-facing tables and figures.

## Requirements

- Docker
- Bash
- At least several GB of free disk space for the Docker image

The analysis image is built from `programs/code/Dockerfile.base` using the pinned R environment in `programs/code/renv.lock`.

## Docker Demo

Build the Docker image:

```bash
docker build -f programs/code/Dockerfile.base -t fedpoppk/deferiprone:20260521 programs/code
```

Run a single federated fit demo:

```bash
bash programs/entrypoints/demo_docker.sh
```

By default, the demo starts three client services and one server service, runs a short Scenario 1 federated base-model execution check using the included site-wise input data, and writes outputs to `results/demo/scenario1_federated/`. It uses `DEMO_MAXIT=1` and a relaxed `DEMO_PGTOL` for quick verification, and does not rerun bootstrap.

To run Scenario 2 instead:

```bash
SCENARIO=scenario2 bash programs/entrypoints/demo_docker.sh
```

To use more optimizer iterations and the manuscript optimization tolerance in the demo:

```bash
DEMO_MAXIT=100 DEMO_PGTOL=0 bash programs/entrypoints/demo_docker.sh
```
