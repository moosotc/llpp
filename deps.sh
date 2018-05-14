#!/bib/bash

MUPDF_OUTPUT_DIR="mupdf"
MUPDF_URL="https://github.com/ArtifexSoftware/mupdf"
MUPDF_DESIRED_VERSION="bd88b96f7b7b21d64d4fb32a2674653a08f8cb38"

if [ ! -d ${MUPDF_OUTPUT_DIR} ]; then
	echo "mupdf does not exist, fetching it from ${MUPDF_URL}"
	git clone ${MUPDF_URL} --recursive ${MUPDF_OUTPUT_DIR}
fi

cd ${MUPDF_OUTPUT_DIR}
MUPDF_VERSION=$(git rev-parse HEAD)

if [ "${MUPDF_VERSION}" = "${MUPDF_DESIRED_VERSION}" ]; then
	exit 0
fi

echo "mupdf current version is ${MUPDF_VERSION} switching to ${MUPDF_DESIRED_VERSION}"
git checkout ${MUPDF_DESIRED_VERSION}
git submodule update --recursive
make build=native libs

cd ..
