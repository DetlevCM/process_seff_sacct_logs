


####
#### Settings
####

# force rewriting of csv files, else skip if they exist:
force_recreate_csv=FALSE

# unpack day archive and delete result (better performance)
unpack_day_archive=TRUE

# unpack sacct archive
unpack_sacct_archive = FALSE

# can the script run in parallel? 
run_script_in_parallel=TRUE

####
#### End Settings
####



####
#### Pre-Run Checks
####

# verify if the required packages are present: https://www.statology.org/r-check-if-package-is-installed/:

if (system.file(package='archive') == '')
{
# archives must be unpacked without the archive package using system tools
unpack_sacct_archive = TRUE
unpack_day_archive = TRUE
}

if (system.file(package='foreach') == '' || system.file(package='doParallel') == '')
{
# parallelism depends on both foreach and doParallel
run_script_in_parallel = FALSE
}

####
#### End Pre-Run Checks
####



####
#### Load Libraries
####

# conditionally load archive reading if active
if(!unpack_sacct_archive)
{
## archive is required to read files from archives directly
library(archive)
}

# conditionally load parallelisation if active
if(run_script_in_parallel)
{
# https://stackoverflow.com/questions/66295800/why-my-r-code-is-not-parallel-cpu-when-using-foreach
library(doParallel)
library(foreach)
no_cores <- detectCores()
registerDoParallel(cores = no_cores - 4)
}

####
#### Loaded Libraries
####



####
#### Start actual Processing
####

# obtain an overview of all days for which there is data:
# https://stackoverflow.com/questions/14958516/read-all-files-in-directory-and-apply-multiple-functions-to-each-data-frame
GetAllDays <- function()
{
files <- list.files(path=".", pattern="*.sacct.log.bz2", full.names=TRUE, recursive=FALSE)
#print(files)
}



# process efficieny data for a specific job return a vecctor of data
GetEfficicencies <- function(day,job_id){

## example data pattern

# for ever id.seff ->
#
# Job ID: 808144
# Array Job ID: 719998_100
# Cluster: pool
# User/Group: username/eins
# State: RUNNING
# Nodes: 1
# Cores per node: 32
# CPU Utilized: 00:00:00
# CPU Efficiency: 0.00% of 26-15:22:40 core-walltime
# Job Wall-clock time: 19:58:50
# Memory Utilized: 0.00 MB (estimated maximum)
# Memory Efficiency: 0.00% of 64.00 GB (2.00 GB/core)
# WARNING: Efficiency statistics may be misleading for RUNNING jobs.


# reading files from an archive can be slow, would this be faster working on unzipped files?

data_raw = NULL

# Pattern: job_file = "2025-06-19/700313.seff"
job_file = paste(day,paste(job_id,"seff",sep="."),sep = "/")
# Pattern: day_archive = "2025-06-19.tar.bz2"
day_archive = paste(day,"tar.bz2",sep=".")

if(dir.exists(day)) # read from directory (fast)
{
data_raw = readLines(job_file)
}
else if(file.exists(day_archive)) # read from archive (slow)
{
# print(c(day_archive,job_file))
# https://cran.r-project.org/web/packages/archive/readme/README.html
# https://www.tidyverse.org/blog/2021/11/archive-1-1-2/

# reading the file from the archive
archive_data_connection = archive_read(day_archive, file = job_file)
data_raw = readLines(archive_data_connection)
}
else
{
warning("Job Efficiency Data not found!")
return(rep(NA,length=5)) ## make this the length of extracted variables to fill NA for missing data
}

# hypothesis: data never changes format
# https://www.r-bloggers.com/2024/08/mastering-grep-in-r-a-fun-guide-to-pattern-matching-and-replacement/


# multiple steps: 1: find the line, 2: split the line 3: save the right position in the slit line
# cpu_line = grep("CPU Efficiency",data_raw)
# cpu = unlist(strsplit(data_raw[cpu_line],split = " "), use.names=FALSE)
# cpu = unlist(strsplit(data_raw[cpu_line],split = " "), use.names=FALSE)[3]

SplitLine <-function(data){
    unlist(strsplit(data,split = " "), use.names=FALSE)
}

cpu = SplitLine(data_raw[grep("CPU Efficiency:",data_raw)])[3]
mem = SplitLine(data_raw[grep("Memory Efficiency:",data_raw)])[3]
nbr_nodes = SplitLine(data_raw[grep("Nodes:",data_raw)])[2]
cpu_utilized = SplitLine(data_raw[grep("CPU Utilized:",data_raw)])[3]
cores_per_node = SplitLine(data_raw[grep("Cores per node:",data_raw)])[4]

# remove percent & make numeric where appropriate
cpu = as.numeric(gsub("%", "", cpu))
mem = as.numeric(gsub("%", "", mem))
## not cpu_utilized
nbr_nodes = as.numeric(nbr_nodes)
cores_per_node = as.numeric(cores_per_node)

# return a vector, cpu & mem Efficiency
result = c(cpu,mem, nbr_nodes, cpu_utilized, cores_per_node)
}
## end GetEfficicencies()



## process data for a single day of sacct logs
## writes csv file with resulting data for each day
ProcessDay <- function(day,unpack_sacct_archive,run_script_in_parallel)
{


data_prepared <- NULL # prepare variable
# user does not have the archive package, thus unpack, process, delete:
if(unpack_sacct_archive)
{
# needs to use the system command...
unpack <- paste("bunzip2 -k",paste(day,"sacct.log.bz2",sep="."),sep=' ')
system(unpack)
data_raw = readLines(paste(day,"sacct.log",sep="."))
unlink(paste(day,"sacct.log",sep="."))
data_prepared = data_raw[-2]
}
else
{
# https://stackoverflow.com/questions/15860071/read-csv-header-on-first-line-skip-second-line
sacct_day = paste(day,"sacct.log.bz2",sep=".")
data_raw = readLines(bzfile(sacct_day))
data_prepared = data_raw[-2]
}

# Replace "None assigned" with a phrase without a white space
# https://www.geeksforgeeks.org/r-language/replace-specific-characters-in-string-in-r/
data_prepared = gsub("None assigned", "None_assigned", data_prepared)

# read in the data
jobs <- read.csv(textConnection(data_prepared),header=TRUE,sep="")


## expand headers by data to be read from seff file
# get the column names
headers = colnames(jobs)
headers = c(headers, "cpu_eff", "mem_eff", "nbr_nodes", "cpu_utilized", "cores_per_node")

# write back headers
cpu_eff <- vector(,length=nrow(jobs))
mem_eff <- vector(,length=nrow(jobs))
nbr_nodes <- vector(,length=nrow(jobs))
cpu_utilized <- vector(,length=nrow(jobs))
cores_per_node <- vector(,length=nrow(jobs))
jobs = cbind(jobs,cpu_eff,mem_eff,nbr_nodes,cpu_utilized,cores_per_node)
names(jobs) <- headers
## end expand headers by data to be read from seff file


## filter out jobs that did not run (runtime 0-5 seconds)
jobs <- subset(jobs, jobs$Elapsed != "00:00:00") #
jobs <- subset(jobs, jobs$Elapsed != "00:00:01") #
jobs <- subset(jobs, jobs$Elapsed != "00:00:02") #
jobs <- subset(jobs, jobs$Elapsed != "00:00:03") #
jobs <- subset(jobs, jobs$Elapsed != "00:00:04") #
jobs <- subset(jobs, jobs$Elapsed != "00:00:05") #


# R subsets:
# # https://www.geeksforgeeks.org/r-language/how-to-conditionally-remove-rows-in-r-dataframe/
# remove jobs that never ran:
jobs <- subset(jobs, jobs$NodeList != "None_allocated") #
# remove running jobs
jobs <- subset(jobs, jobs$State != "RUNNING") #








# loops in R are slow: get data for each job

if(run_script_in_parallel) # run in parallel
{
efficiencies <- foreach(i=1:nrow(jobs)) %dopar% GetEfficicencies(day,jobs$JobID[i])

for ( i in 1:nrow(jobs) ) {
cpu_mem =  unlist(efficiencies[i])
jobs$cpu_eff[i] = cpu_mem[1]
jobs$mem_eff[i] = cpu_mem[2]
jobs$nbr_nodes[i] = cpu_mem[3]
jobs$cpu_utilized[i] = cpu_mem[4]
jobs$cores_per_node[i] = cpu_mem[5]
}
}
else # or in serial
{
for ( i in 1:nrow(jobs) ) {
efficiency = GetEfficicencies(day,jobs$JobID[i])

cpu_mem =  unlist(efficiency)
jobs$cpu_eff[i] = cpu_mem[1]
jobs$mem_eff[i] = cpu_mem[2]
jobs$nbr_nodes[i] = cpu_mem[3]
jobs$cpu_utilized[i] = cpu_mem[4]
jobs$cores_per_node[i] = cpu_mem[5]
}
}

## subset statistics to be removed later
if(FALSE){
jobs_cancelled <- subset(jobs, jobs$State == "CANCELLED+")
jobs_completed <- subset(jobs, jobs$State == "COMPLETED") # || jobs$State == "FAILED" ) #, State = "COMPLETED", State = "FAILED", State = "CANCELLED" ]
jobs_failed <- subset(jobs, jobs$State == "FAILED") #
# summary(jobs_completed) provides a first statistic
#print("completed:")
#print(summary(jobs_completed))
#print("cancelled:")
#print(summary(jobs_cancelled))
#print("failed:")
#print(summary(jobs_failed))
}

#  row.names=FALSE -> no counter for each row
write.csv(jobs,file=paste(day,"csv",sep="."),  row.names=FALSE)
}



### ## ## ## ## ## ## ## ## ## ## ## ## ##
###                                     ##
### use the functions to process data ####
###                                     ##
### ## ## ## ## ## ## ## ## ## ## ## ## ##



## get all days
all_days <- GetAllDays()


## loop over all days
for ( i in 1:length(all_days) ){

# day = "2025-06-19"
# the day file name format is fixed:
# https://www.r-bloggers.com/2023/08/the-substring-function-in-r/
day <- substring(all_days[i],3,12)

## unless forced, we can skip existing day data
if(!file.exists(paste(day,"csv",sep='.')) || force_recreate_csv)
{
print(c(i," processed: ",day))
# we can unpack the day archive to improve performance:
if(unpack_day_archive)
{
# unpack
untar(paste(day,"tar.bz2",sep="."))
# process
ProcessDay(day,unpack_sacct_archive,run_script_in_parallel)
# clean
unlink(day, recursive=TRUE)
}
else
{
## process the day
ProcessDay(day,unpack_sacct_archive,run_script_in_parallel)
}
}
else
{
print(c(i, " skipped: ",day))
}
}





