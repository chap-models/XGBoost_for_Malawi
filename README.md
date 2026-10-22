# XGBoost_for_Malawi

A CHAP model based on code from local modellers in Malawi. It trains one
XGBoost model per region on monthly data, with rainfall and mean temperature as
predictors, and produces prediction samples by bootstrapping the training data.

The model is packaged as a [chapkit](https://github.com/dhis2-chap/chapkit)
service: it runs in its own container, built from
`ghcr.io/dhis2-chap/chapkit-r` with the R packages pinned in `renv.lock`, and
registers itself with CHAP. It does not need R in the CHAP worker image.

## Build and test locally

Requires Docker and [uv](https://docs.astral.sh/uv/).

```bash
make test          # build the image, start it, run `chapkit test`, tear down
make docker-run    # build and serve on http://localhost:9090
```

See [`CHAPKIT.md`](CHAPKIT.md) for the full list of targets and the service API.

## Run with CHAP

Images are published to `ghcr.io/chap-models/xgboost_for_malawi` on every push
to `main` (`:latest`) and every `v*.*.*` tag. From a CHAP Docker Compose
deployment directory:

```bash
chap-admin install xgboost-for-malawi --image ghcr.io/chap-models/xgboost_for_malawi:latest --accept-risk
```

To pin a release, replace `latest` with a version tag such as `0.1.0`.

This starts the service, lets it register with CHAP and adds a default
configuration, so the model appears in the Modeling App. See
[Running Your Own Model](https://dhis2-chap.github.io/chap-core/modeling-app/running-your-own-model/)
in the CHAP docs for the manual Compose setup.

## Updating the R dependencies

`renv.lock` pins the R packages. After changing a package, regenerate it inside
the base image so the versions match what the Dockerfile restores:

```bash
docker run --rm -v "$PWD":/work -w /work ghcr.io/dhis2-chap/chapkit-r:latest \
  Rscript -e 'renv::install(c("dplyr", "xgboost")); renv::snapshot(prompt = FALSE)'
```

Bump `version` in `main.py` whenever the model changes: CHAP stores a model
template per reported version and refuses a rebuilt image under a version it
already has.
