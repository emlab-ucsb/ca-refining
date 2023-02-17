# Distributional and climate impacts of low-carbon transition pathways for California's oil refining (repo)

## Setting up
This repo relies on the R package ``targets`` to maintain the pipeline of the scripts and the reproducibility of the project.
Install the package if you have not already done so:

```
install.packages("targets")
```

Load the package:

```
library(targets)
```

All of the functions for the pipeline are in the ``R/`` folder.
To open the ``_targets.R`` script (which is where the workflow is built and specified), run:

```
tar_edit()
```

**IMPORTANT**: Before running the pipeline, one thing needs to be changed -- the path to the ``calepa-cn`` folder. 
Look for the ``main_path`` target:

```
tar_target(name = main_path, command = "/Volumes/GoogleDrive-103159311076289514198/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn"),
```

And replace the path above with your own path that points to the calepa-cn folder. Everything is relative to this one main path.

### Debugging the pipeline
In order to check the pipeline is engineered properly, run the following command:

```
tar_manifest(fields=command)
```

The output should look something like:
```
# A tibble: 90 × 2
   name          command                                                                                                             
   <chr>         <chr>                                                                                                               
 1 ei_crude      "5.698"                                                                                                             
 2 ei_diesel     "5.77"                                                                                                              
 3 ei_gasoline   "5.052"                                                                                                             
 4 ei_jet        "(5.67 + 5.355)/2"                                                                                                  
 5 clus          "c(\"North\", \"South\")"                                                                                           
 6 main_path     "\"/Volumes/GoogleDrive-103159311076289514198/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn\""
 7 pred_years    "2020:2045"                                                                                                         
 8 ref_threshold "0.6"                                                                                                               
 9 drop_in_perc  "1"                                                                                                                 
10 ref_scens     "c(\"historic exports\", \"historic production\", \"low exports\")"                                                 
# … with 80 more rows
# ℹ Use `print(n = ...)` to see more rows
```
If there are any issues (missing targets, bugs, etc), you should receive an error message.

## Running the pipeline

To build and run the pipeline (this will execute everything), run:

```
tar_make()
```

If you are running this for the first time, it should take a few minutes, but the outputs should look something like:

```
• start target ei_crude
• built target ei_crude [0.019 seconds]
• start target ei_diesel
• built target ei_diesel [0 seconds]
• start target ei_gasoline
• built target ei_gasoline [0 seconds]
• start target ei_jet
• built target ei_jet [0.001 seconds]
• start target clus
• built target clus [0 seconds]
• start target main_path
• built target main_path [0 seconds]
...
```

Assuming none of the targets change, the next time(s) you run ``tar_make()``, ``targets`` will skip building targets that are already up-to-date. 

## Viewing and loading targets

If you are new to ``targets`` you might be confused that there are no objects in your environment. That's because the objects are stored locally in a folder called ``_targets`` (in your local repo).

But let's say you want to inspect a specific object, like ``dt_its``. If you want to just view it in your console, you can enter:

```
tar_read(dt_its)
```

And that should print the ``data.table``.

If you want to load the ``data.table`` into your environment, you can run the following instead:

```
tar_load(dt_its)
```

You'll notice the object is in your environment.

You can also view plots. Running the following line should either load the plot in your Plots window or open a new window with the plot:

```
tar_read(fig_demand)
```

## Visualizing the pipeline

If you want to visualize the pipeline, run:

```
tar_visnetwork()
```

You'll notice the diagram is very small -- you can use your mouse to zoom in on the objects if you'd like. If you make changes to the targets/pipeline and run ``tar_visnetwork()`` before running ``tar_make``, you can see the colors of the objects change.