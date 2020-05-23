#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# Name          :03_cleanup_install.sh
# Description   :Clean up the data-raw directory and install and rebuild package.
# Author        :Merlin Unterfinger <info@munterfinger.ch>
# Date          :2020-05-23
# Version       :0.1.0
# Usage         :./03_cleanup_install.sh
# Notes         :
# R             :4.0.0
# =============================================================================

# Remove datasets
rm -rf statpop statent

# Reinstall package
cd ..
R CMD INSTALL .
cd -
