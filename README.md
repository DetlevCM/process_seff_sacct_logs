# Process Seff Sacct Logs

A collection of R-scripts to help analyse job efficiencies, to obtain a better overview of the "quality" of user submitted jobs with respect to requested ressources in a SLURM cluster.
To run the scripts, data needs to be collected from both `sacct` and `seff`.

A small number of R libraries are employed - notably to help with performance by running the reading step in parallel.
Serial execution is possible, however will be slower.

## Dependencies

```R
install.packages("archive")
install.packages("foreach")
install.packages("doParallel")
```

## How to Run

- collect the data in a predefined pattern (TBD if script will be part of the repo)
- collect data from the archives and log into readable csv files  
     `Rscript read_data.R`
- collate data into a single data source (csv file)  
     `Rscript collate_data.R`
- process data  
     `Rscript try_plotting.R`
