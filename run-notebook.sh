#! /bin/bash

export DYLD_LIBRARY_PATH=$(cd deps/ipython-xlang-kernel;pwd)
export KERNEL_BIN=$(pwd)/tjipython
export NOTEBOOK_DIR=$(pwd)/notebooks
export IPYTHON_HOME=$(cd deps/ipython; pwd)

if ! [ -d ${NOTEBOOK_DIR} ] ; then
    mkdir ${NOTEBOOK_DIR}

fi
pushd ${NOTEBOOK_DIR}
cp ${IPYTHON_HOME}/kernel.sh ${NOTEBOOK_DIR}
python ${IPYTHON_HOME}/ipython.py notebook
popd
