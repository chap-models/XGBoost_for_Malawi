"""A general XGBoost model fitted individually for each region."""

import os
from pathlib import Path

from chapkit import BaseConfig
from chapkit.api import AssessedStatus, MLServiceBuilder, MLServiceInfo, ModelMetadata, PeriodType
from chapkit.artifact import ArtifactHierarchy
from chapkit.ml import ShellModelRunner
from pydantic import Field


class XGBoostForMalawiConfig(BaseConfig):
    """Configuration for XGBoost_for_Malawi."""

    prediction_periods: int = Field(
        default=3,
        description="Number of periods to predict into the future.",
    )

    # The scripts use rainfall and mean_temperature as fixed predictors; both are
    # required covariates below, so there are no additional covariates to configure.


# Commands are rendered in ShellModelRunner's template vocabulary
# ({data_file}, {historic_file}, {future_file}, {output_file}, {geo_file}).
# ShellModelRunner substitutes those with workspace-root filenames at train /
# predict time. Literal paths `model` (your script saves/loads it; preserved
# across train -> predict via workspace copy) and `config.yml` (always written
# by chapkit from the config above) stay verbatim.
# config_format="chap_core" writes config.yml in chap-core's ModelConfiguration
# shape (prediction_periods + additional_continuous_covariates at the top level,
# remaining fields nested under user_option_values) so scripts ported from
# chap-models repos can read config["user_option_values"][...] unchanged.
runner: ShellModelRunner[XGBoostForMalawiConfig] = ShellModelRunner(
    train_command="Rscript train.R {data_file} model",
    predict_command="Rscript predict.R model {historic_file} {future_file} {output_file}",
    config_format="chap_core",
    # MLproject adapters: extra columns added to the data, historic and future CSVs
    # (new column -> source column, or year / month / week derived from time_period).
    adapters={'Cases': 'disease_cases', 'E': 'population', 'month': 'month', 'week': 'week', 'ID_year': 'year', 'ID_spat': 'location'},
)

info = MLServiceInfo(
    id="xgboost-for-malawi",
    display_name="XGBoost",
    version="1.0.0",
    description="A general XGBoost model fitted individually for each region.",
    model_metadata=ModelMetadata(
        author="Halvard Emil Sand-Larsen",
        author_note="This model handles each region individually and it expects monthly data for all the covariates.",
        author_assessed_status=AssessedStatus.red,
        contact_email="halvard@dhis2.org",
        organization="HISP Centre, University of Oslo",
        organization_logo_url="https://landportal.org/sites/default/files/2024-03/university_of_oslo_logo.png",
        citation_info="Climate Health Analytics Platform. 2025. \"XGBoost\". HISP Centre, University of Oslo. https://dhis2-chap.github.io/chap-core/external_models/overview_of_supported_models.html",
    ),
    period_type=PeriodType.monthly,
    required_covariates=["population", "rainfall", "mean_temperature"],
)

hierarchy = ArtifactHierarchy(
    name="XGBoost_for_Malawi",
    level_labels={0: "ml_training_workspace", 1: "ml_prediction"},
)

# Persistent SQLite database by default so artifacts, configs, and jobs
# survive container restarts. Override with DATABASE_URL for a different
# SQLite file or a hosted Postgres (e.g. when deployed next to chap-core).
DATABASE_URL = os.getenv("DATABASE_URL", "sqlite+aiosqlite:///data/chapkit.db")
if DATABASE_URL.startswith("sqlite") and ":///" in DATABASE_URL:
    db_path = Path(DATABASE_URL.split("///")[1])
    db_path.parent.mkdir(parents=True, exist_ok=True)

app = (
    MLServiceBuilder(
        info=info,
        config_schema=XGBoostForMalawiConfig,
        hierarchy=hierarchy,
        runner=runner,
        database_url=DATABASE_URL,
    )
    # See compose.yml for chap-core self-registration env vars.
    .with_registration()
    .build()
)


if __name__ == "__main__":
    from chapkit.api import run_app

    run_app("main:app", host="0.0.0.0", port=9090)