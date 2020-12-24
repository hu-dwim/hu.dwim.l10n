#!/bin/bash

#
# Parameters
#
CLDR_CORE_URL="http://unicode.org/Public/cldr/30.0.2/core.zip"
LISP=sbcl

#
# Script body
#
set -e

SCRIPT_DIR=`dirname "$0"`
SCRIPT_DIR=`readlink -f ${SCRIPT_DIR}`

cd ${SCRIPT_DIR}

if [ ! -e "./xml/common/" ]; then
    pushd "xml/"
    wget --continue ${CLDR_CORE_URL}
    unzip core.zip
    popd
fi

# "call" the lisp part below.
# NOTE: --script implies --no-userinit (i.e. no quicklisp from .sbclrc), so we use a different trick here to skip the first line.
exec ${LISP} --dynamic-space-size 1000 --noinform --end-runtime-options \
  --eval "(declaim (optimize debug))" \
  --eval "(require :asdf)" --eval "(asdf:load-system :asdf)" \
  --eval "(asdf:load-system :hu.dwim.cldr-compiler)" \
  --eval "(hu.dwim.cldr-compiler:compile-cldr-files)"
  --end-toplevel-options
