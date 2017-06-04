#!/usr/bin/python

### Copyright (C) 2017 Rafael Laboissière
###
### This program is free software: you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by the
### Free Software Foundation, either version 3 of the License, or (at your
### option) any later version.
###
### This program is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License along
### with this program.  If not, see <http://www.gnu.org/licenses/>.

import os
import sys
import fnmatch

### Get the program name and the current directory
prog = os.path.basename (sys.argv [0])
cur_dir = os.path.dirname (sys.argv [0])

### Get the directory with the results
results_dir = os.path.join (cur_dir, "..", "results")

### Find files
info_files = []
for root, dirnames, filenames in os.walk (results_dir):
    for filename in fnmatch.filter (filenames, 'info.yaml'):
        print root
