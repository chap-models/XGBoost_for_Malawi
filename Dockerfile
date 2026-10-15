FROM ghcr.io/dhis2-chap/chapkit-r:latest

# Build steps run as root; the service itself runs as the unprivileged `chapkit`
# user (the switch happens right before CMD). Same posture as chap-core's own
# images. Newer chapkit base images ship the user already (uid/gid 1000); older
# ones do not, so create it only when it is missing.
USER root
RUN id -u chapkit >/dev/null 2>&1 \
    || (groupadd --gid 1000 chapkit && useradd --uid 1000 --gid 1000 --no-create-home --shell /usr/sbin/nologin chapkit)

WORKDIR /work
# Copy lockfile + manifest first so the dep-install layer caches independently of code changes.
# Run `uv sync` once locally before the first `docker build` to generate uv.lock.
COPY pyproject.toml uv.lock ./

# Sync runtime deps (chapkit + the user's pyproject deps) into the base image's
# venv at /app/.venv. The chapkit-{py,r,r-inla}:latest images no longer ship
# chapkit pre-installed (see chapkit-images), so this step is required for the
# generated main.py to import chapkit at runtime. --frozen pins to uv.lock for
# reproducible builds, --no-dev skips dev-only deps (uvicorn etc. arrive
# transitively through chapkit), --no-install-project because this isn't a
# package, just a service.
RUN --mount=type=cache,target=/root/.cache/uv \
    UV_PROJECT_ENVIRONMENT=/app/.venv uv sync --frozen --no-dev --no-install-project

# Git commit the image was built from, exposed as git_revision on /api/v1/info. Same
# convention as chap-core: pass --build-arg GIT_REVISION=$(git rev-parse HEAD) (the
# Makefile docker-build target and compose.yml already do). Declared after the uv sync
# layer so a new commit does not invalidate the dependency cache.
ARG GIT_REVISION=""
ENV GIT_REVISION=${GIT_REVISION}

# Restore the R library declared in renv.lock at image build time so the
# container starts instantly instead of bootstrapping renv on every run.
# Only the lockfile is copied first, so the (slow, xgboost compiles from source)
# restore layer is cached independently of script changes.
# Installs renv first if the base image doesn't ship it; no-op otherwise.
# The renv cache goes under /opt (world-readable) instead of root's home, because
# the project library symlinks into it and the service runs as a different user.
# The sandbox is disabled so renv does not try to write one at startup.
COPY renv.lock ./
ENV RENV_PATHS_CACHE=/opt/renv/cache \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE
RUN R -q -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore(prompt = FALSE)"

# Migrated projects ship arbitrary user files (R scripts, helper modules, config
# YAMLs, ...) at the root, so we copy the whole tree rather than enumerating
# (`COPY main.py ./` + `COPY scripts/` like the scaffold does for its known layout).
COPY . /work
RUN test ! -d /work/renv || chown -R chapkit:chapkit /work/renv

# The service writes in exactly two places: /work/data (SQLite database, a named
# volume in compose.yml) and /tmp (ML workspaces, a tmpfs in compose.yml). Everything
# else stays read-only, which is what compose.yml's `read_only: true` enforces. HOME,
# MPLCONFIGDIR and XDG_CACHE_HOME point libraries that want a scratch directory
# (R session files, matplotlib, pip/uv caches) at /tmp as well.
RUN mkdir -p /work/data && chown -R chapkit:chapkit /work/data
ENV HOME=/tmp \
    MPLCONFIGDIR=/tmp \
    XDG_CACHE_HOME=/tmp/.cache
USER chapkit

EXPOSE 8000

HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl --fail http://localhost:8000/health || exit 1

# Run uvicorn directly so server flags (--workers, --log-level, --timeout-keep-alive,
# etc.) are visible here in the Dockerfile. main.py's `if __name__ == "__main__":`
# block stays as the local-dev shortcut for `python main.py`.
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]