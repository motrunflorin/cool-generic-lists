#!/bin/bash

cp *.cl ../checker/sources/

cd ../checker
python3 checker.py
cd ../schelet