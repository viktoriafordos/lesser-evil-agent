source ~/repos/confd-7.2/env.sh

set -x

BASHO_BENCH=/Users/vfordos/repos/basho_bench/_build/default/bin/basho_bench
BASE_DIR="/tmp/results-$(date +%s)"

mkdir -p "${BASE_DIR}"

for test in `ls experiments`; do
    TEST_DIR="${BASE_DIR}/${test}"
    mkdir -p "${TEST_DIR}"
    for sut in "sut" "sut-wo"; do
        SUT_DIR="${TEST_DIR}/${sut}"
        mkdir -p "${SUT_DIR}"
        cp experiments/${test} ${SUT_DIR}/config
        if [ "sut-wo" = ${sut} ]; then
            touch ${SUT_DIR}/no_lesser_evil
        fi
        CONTID=`docker run -d --memory="500m" --kernel-memory="500m" --mount type=bind,source=${SUT_DIR},target=/measurements -p 8888:8888 --init ${sut}`
        while [ "       4" != "`ls -l ${SUT_DIR} | wc -l`" ]; do
            sleep 1
        done
        ${BASHO_BENCH} "experiments/${test}"
        docker stop ${CONTID}
    done
done