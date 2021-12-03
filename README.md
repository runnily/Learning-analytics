# learning_analysis

Welcome to ProjectTemplate!

This files species how you should use ProjectTemplate.

Before we begin please by start by setting  your setwd() into the directory where the 
"setup.r" is located. Once done please run the stepup.r libraries to download all 
the relevant files and begin the project.

  stepup.r
  
Once you've done this, you'll see a series of automated
messages as ProjectTemplate goes about doing its work. This work involves:
* Reading in the global configuration file contained in `config`.
* Loading any R packages you listed in the configuration file.
* Reading in any datasets stored in `data` or `cache`.
* Preprocessing your data using the files in the `munge` directory.

Once that's done, you can execute any code you'd like. For every analysis
you create, we'd recommend putting a separate file in the `src` directory.
If the files start with the two lines mentioned above:

	library('ProjectTemplate')
	load.project()

You'll have access to all of your data, already fully preprocessed, and
all of the libraries you want to use.

For more details about ProjectTemplate, see http://projecttemplate.net
