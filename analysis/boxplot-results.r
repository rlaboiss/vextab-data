###
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

### * Boxplot the results

### ** All conditions, collapsed objects

### *** Threshold
pdf (file = "obj-stab-threshold-all.pdf")
par (mar = c (11, 4, 1, 1))
boxplot (threshold ~ chair * stimulus * background, data = obj.stab.psycho,
         las = 2, ylab = "threshold (degree)")
dev.off ()

### *** Slope
pdf (file = "obj-stab-slope-all.pdf")
par (mar = c (11, 4, 1, 1))
### **** Inclination at threshold is 25*a
boxplot (abs (25 * slope) ~ chair * stimulus * background, log = "y",
         data = obj.stab.psycho, las = 2, ylab = "slope (%/degree)")
dev.off ()

### ** Object condition
df <- droplevels (subset (obj.stab.psycho, stimulus == "object"))

### *** Threshold
pdf (file = "obj-stab-threshold-object.pdf")
par (mar = c (11, 4, 1, 1))
boxplot (threshold ~ object * chair * background, data = df,
         las = 2, ylab = "threshold (degree)")
dev.off ()

### *** Slope
pdf (file = "obj-stab-slope-object.pdf")
par (mar = c (11, 4, 1, 1))
### **** Inclination at threshold is 25*a
boxplot (abs (25 * slope) ~ object * chair * background, log = "y",
         data = df, las = 2, ylab = "slope (%/degree)")
dev.off ()
