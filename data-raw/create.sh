#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# Name          :create.sh
# Description   :Downloads the needed data sets, creates the example R objects
#                and saves them to the 'inst' directory of the package. Finally
#                the 'data-raw' directory is cleaned and the 'drtplanr' package
#                is reinstalled.
# Author        :Merlin Unterfinger <info@munterfinger.ch>
# Date          :2020-05-23
# Version       :0.1.0
# Usage         :./create.sh -k <HERE-API-KEY>
# Notes         :Run from the 'data-raw' directory and provide a HERE API key
#                using the -k option.
# Bash          :5.0.17
# =============================================================================

# Parsing script options with getopts
while getopts ":hk:" opt; do
  case ${opt} in
    h )
      echo "Usage:"
      echo "    create.sh -k <HERE-API-KEY>     Pass a HERE API key."
      exit 0
      ;;
    k )
      key=$OPTARG
      ;;
    \? )
      echo "Invalid option: $OPTARG" 1>&2
      exit 1
      ;;
    : )
      echo "Invalid option: $OPTARG requires an argument" 1>&2
      exit 1
      ;;
  esac
done
if [ $OPTIND -eq 1 ]; then
  echo "Error: No options were passed, see -h for help."; exit 1
fi
shift $((OPTIND -1))

# Download data sets
./01_download_extract.sh

# Create R objects (and passing HERE API key)
cd ..
./data-raw/02_create_examples.R $key
cd -

# Cleanup and reinstall
./03_cleanup_install.sh
