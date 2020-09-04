#!/bin/bash

set -e

set -a
# shellcheck disable=SC1091
source .env
set +a

fake build --target run
# cd deploy && dotnet Server.dll