#
# Author:     Jesse Lecy
# Maintainer: Cristian Nuno
# Date:       March 21, 2021
# Purpose:    Create custom functions to pre-process the LTDB raw data files
#

# load necessary functions ----
# note: all of these are R objects that will be used throughout this .rmd file
import::here("build_year",
             "RELEVANT_FILES",
             "obtain_crosswalk",
             "create_final_metadata_file",
             # notice the use of here::here() that points to the .R file
             # where all these R objects are created
             .from = here::here("labs/wk03/utilities.R"),
             .character_only = TRUE)

# for each relevant file, run the build_year() function ----
# note: this populates the data/rodeo/ directory with clean files
for (relevant_file in RELEVANT_FILES) {
  print(paste0("Starting on ", relevant_file[["year"]]))
  build_year(fn1 = relevant_file[["fullcount"]],
             fn2 = relevant_file[["sample"]],
             year = relevant_file[["year"]])
  if (relevant_file[["year"]] < 2010) {
    print("Finished! Moving onto the next decade.")
  } else {
    print("Finished! No more data to parse.")
  }
}

# load the crosswalk ----
# note: this stores a copy in the data/raw/ directory
cw <- obtain_crosswalk()

# create the final meta data file ----
# note: this stores a copy within the data/rodeo/ directory
create_final_metadata_file(file_names = RELEVANT_FILES,
                           crosswalk = cw)

# end of script #
