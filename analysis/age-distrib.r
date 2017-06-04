###
### Copyright (C) 2017  Rafael Laboissière
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

sessions <- read.csv ("obj-stab-info.csv")
subjects <- read.csv ("cohort-info.csv")

exp.lab <- list ()
exp.lab [["Experiment 1"]] <- "room-126"
exp.lab [["Experiment 2"]] <- "scene-mirror"
exp.lab [["Experiment 3"]] <- "no-table"

for (i in names (exp.lab)) {
    x <- unique (as.character (subset (sessions,
                                       experiment == exp.lab [[i]])$subject))
    y <- subset (subjects, subject %in% x & subject != "S066")
    n <- nrow (y)
    min.age <- floor (min (y$age))
    max.age <- ceiling (max(y$age))
    idx <- sort (y$age, index.return = TRUE)$ix
    pdf (file = sprintf ("%s.pdf", i))
    plot (y$age [idx], seq (1 : n), pch = 19, yaxt = "n", xaxt = "n", main = i,
          xlab = "age (years)", ylab = "", xlim = c (min.age, max.age))
    axis (2, at = seq (1, n), labels = y$subject [idx], las = 1)
    axis (1, at = seq (min.age, max.age))
    dev.off ()
}
