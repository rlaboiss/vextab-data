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

obj-stab-psycho.csv: obj-stab-resp.csv psychometric-estimates.r
	Rscript psychometric-estimates.r

obj-stab-resp.csv: sessions-list.txt
	if [ "$$(md5sum sessions-list.txt)" != "$$(cat sessions-list.md5)" ] ; then	\
	    ./build-database.py ;							\
	    md5sum sessions-list.txt > sessions-list.md5 ;				\
	fi

sessions-list.txt: FORCE
	./list-sessions.py > sessions-list.txt

.PHONY: FORCE
FORCE:

.PHONY: clean
clean:
	rm -f sessions-list.md5 sessions-list.txt
