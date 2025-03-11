# This script runs the 39-Box stage model

# To reset the plot window to default settings and clear all plots 
#   use command dev.off() in the console.

# remove all variables from the environment except "filename"
rm(list = ls()[! ls() %in% "filename"])

# Load the base model
  # Load required dataframes created by script make_datasets.R
  if (file.exists(file="../Datasets/39-Box-Datasets.Rdata")) {
    load(file="../Datasets/39-Box-Datasets.Rdata")
  } else {
    print('Running make_datasets.R to rebuild file 39-Box-Datasets.Rdata.')
    source('make_datasets.R')
  }
  
  # load the general model constants 
  source("39BoxBase.R")
  # functions 
  source("39BoxFunctions.R")           


# set the name of the output file
  if (exists("filename")) { # if the variable "filename" exists
    default_name <- filename
  } else {
    default_name <- '39BoxBase' # file name for the base model run
  }
    user_prompt <- paste("Enter output filename (default is", default_name,"): ")
  user_input <- readline(prompt = user_prompt)
  
  # if the user only pressed enter then use the default file name
  if (user_input == "") {
    filename <- default_name
  } else {
    filename <- user_input
  }
  # set title of this scenario run
  run.title <- filename

# set the name of the main model script, 
  #  modify if a new branch or version of the model is being used
  model.script <- "39BoxStage.R"

# __________________________________________________________
# On the following lines make any changes needed for the current scenario.
  # This may alter constants, loaded dataframes, or redefine functions.
  # Example: To model the impact of increasing flow through the S39 by 20%
  #   NonReg$S39 <- 1.2*NonReg$S39 
  # Save this modified file with a new filename (i.e. not as runStage.R).
  
  # set the S-6 inflow to zero
  Inflow$S6 <- 0
  # set the end date of the simulation to March 30, 2001
    # This is the last day of historic S-6 flow > 0.
  Stop.Date  <- as.Date('2001-03-30')

  
# __________________________________________________________

# run the stage model
  source(model.script)

# run a standard set of graphics 
  source('39BoxStageStdGraphs.R')
  
# clean up
  rm(default_name, user_prompt, user_input, model.script) 
  
# __________________________________________________________
# On the following lines add any final statistical calculations, or graphics

  # run a graphics script
  source("S6ScenarioGraph.R")
  