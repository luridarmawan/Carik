#!/bin/bash
echo "Carik - Run Container (Interactive Shell)"
echo
echo "Direktori project di container: /projects/carik"
echo "Untuk kompilasi manual di dalam container:"
echo "  $ cd /projects/carik"
echo "  $ ./clean.sh"
echo "  $ ./build.sh"
echo

docker run -it --rm -p 8080:80 --name carik-dev carik bash
