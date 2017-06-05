# Data format

The raw outcome of the experiments are stored in the present directory in a
date/time hierachical way, with top-level directories names in the format
`YYYY-MM-DD`, coding for the date, and sub-directories with names in the
format `HH-MM-SS`, coding for the time of the beginning of each sesssion.

In each directory `YYYY-MM-DD/HH-MM-SS`, three files are stored:

- `info.yaml` – Contains the session information, in the following format:
    * subject: *number*
    * background: [ static | vection ]
    * stimulus: [ object | horizontal | egocentric ]
    * chair: [ upright | left | right ]
    * experiment: [ flip-table | new-screen | no-table | room-126 | scene-mirror ]
    * object-side: [ left | right ]
    * table-side: [ left | right | none ]

The *backgound* line indicates whether the surronding of the central scene
is *static* or is moving (*vection*).

The *stimulus* task indicates the type of task that the subject must do,
either the critical angle (CA) estimation task (*object*) or the table
uprightness estimation (TU) task (*horizontal*).  For the *egocentric*
task, the subjects were instructed to say whether the foot of the table was
tilted to the left or to the right in respect to their main body axis.
This condition was done only for the very first subjects and we did not
explored the results.

The *chair* line corresponds to the body orientation.

The *experiment* line tells which main setup was used.  In the manuscript,
we report Experiment 1 as *room-126*, Experiment 2 as *scene-mirror*, and
Experiment 3 as *no-table*.

The *object-side* line corresponds to the side of the scene where the
objects appear.

The *table-side* line corresponds to the side of the scene where the
table appears (*left*, *right*, or *none*, if the table is absent).

- `response.csv` – Comma-separated value (CSV) file with the subject's
  responses, one trial per line, with the following columns:
    * object: the type of the object, in terms of the height of its center
      of mass, *low*, *mid*, or *high* (it is *NA* when the object is not
      shown)
    * stimulus: the stimulus number
    * angle: the tilt angle of the object/table
    * response: the subject's response for the present trial either *left* or
      *right*.  It is *NA* when the subject gives no response.
    * time: the time stamp of the response

- `presentation.csv` – CSV file with the stimuli presentation details, one
  trial per line, with the following columns:
    * object: the type of the object, in terms of the height of its center
      of mass, *low*, *mid*, or *high* (it is *NA* when the object is not
      shown)
    * obj.idx: the stimulus number for the object
    * table: the type of the table shown (it is only *hor*, for the cases of
      interests, so this column can be ignored)
    * tab.idx: the stimulus number for the table
    * time: the time stamp for the beggining of display on the screen
