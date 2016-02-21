#!/usr/bin/env bash

if [ "$EUID" -ne 0 ]
  then echo "Must run as root"
  exit
fi

aptitude update && aptitude install libpq-dev postgresql-client-common postgresql
