#! /bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

sudo apt-get update && sudo apt-get install -qy docker zip

DOCKER_FOLDER=$(mktemp -d)

cp ${DIR}/docker_test/* ${DOCKER_FOLDER}/.
mkdir ${DOCKER_FOLDER}/conda
cp ${DIR}/meta.yaml ${DIR}/build.sh ${DOCKER_FOLDER}/conda/.
sed -i "s/\(\s*\)url:\shttps:\/\/seafile\.lmgc\.univ\-montp2\.fr.*/\1url: \/opt\/project\/lmgc90\_dev\.zip/" ${DOCKER_FOLDER}/conda//meta.yaml

cd ${DIR}/..
zip -r ${DOCKER_FOLDER}/lmgc90_dev.zip --exclude=./build/\* .
cd ${DOCKER_FOLDER}
docker build -f Dockerfile_linux_conda_build -t test_lmgc_conda_package --no-cache .
