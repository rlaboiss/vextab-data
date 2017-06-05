# Data analysis

## Collecting the data from the sessions directories

The Python script [`list-sessions.py`](list-sessions.py) walks recursively
in the results directory looking for session directories (those containing
a file names `info.yaml`, see the [data description](../results/README.md))
and generates a text file called `sessions-list.txt`.  This later file is
processed by the Python script [`build-database.py`](build-database.py),
which generates the comma-separated values (CSV) files `obj-stab-resp.csv`
and `obj-stab-info.csv` files. These files contains, respectively, the
subject responses data, for each trial in each session, and the session
information data, one session per line.

There is a [`Makefile`](Makefile) for automating this procedure.

## Extracting the psychometric parameters

The R script [`psychometric-estimates.r`](psychometric-estimates.r)
extracts the threshold and the slope of the psyhcometric curve fitted to
the response data for each session (and for each object type, in the
critical angle estimation task).  The results are stored in a big CSV file
called `obj-stab-psycho.csv`, which contains the results. one session per
line.  The reaction time is also stored, even though it has not been
explored for now.

## Statistical analysis

The statistical analysis of the results of Exp. 1, 2, and 3 are done in the
R script [`statistical-analysis.r`](statistical-analysis.r).  They are
described in detail in the manuscript.


