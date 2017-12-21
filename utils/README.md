# utils

A bunch of clojure utility functions that I wrote to help me learn and work faster

## Installation

1. Clone the repo
2. Create the following config files:
  * ~/.utils.constants/.utils.constants.clj
    This file will be created, if not present, with the default values the first time
    a repl is created for this project. It can then be updated to correct the values
    accordingly.
  * ~/.ssh/utils.bitbucket.identity
    I know this is not a very secure way to store a username and password, but for now,
    it is what I am using. It should contain the following with the proper values:
      {:user "me@me.com", :password "Hello123"}

3. Start a repl
    $ cd <utils_project_home>
    $ lein do clean, repl

## Usage

### utils.main
Simply a namespace that the repl starts in and has references (required) for all the other namespaces in this project plus other regularly used third party libraries to make it easier to reference each in a single space:
    [utils.core :as utils]
    [utils.constants :as constants]
    [utils.identity]
    [utils.environments :as environments]
    [utils.configuration :as configuration]
    [utils.dependencies :as dependencies]
    [utils.repositories :as repositories]

### [utils.configuration](https://github.com/fergusonsa/utils-clj/blob/master/utils/src/utils/configuration.clj)
Functions for getting, setting, and comparing application configurations that are stored in Zookeeper and in several different file locations.

### [utils.constants](https://github.com/fergusonsa/utils-clj/blob/master/utils/src/utils/constants.clj)
A namespace that contains constant values that are defined in the ~/.utils.constants/.utils.constants.clj file so that they can be referenced by other namespaces. The file is loaded the first time the utils.constants namespace is loaded. Many of the settings are specific to the project or company I am working at, so are set in a non-versioned file.

### [utils.core](https://github.com/fergusonsa/utils-clj/blob/master/utils/src/utils/core.clj)
General and miscellaneous functions, including (but not limited to)
* loading and saving clojure data from and to files
* logging messages to a daily log file
* show help for a given namespace by printing public definitions of variable and functions, including docstrings and parameters
* print differences between 2 maps, showing same sections, differing sections, and unique sections

### [utils.dependencies](https://github.com/fergusonsa/utils-clj/blob/master/utils/src/utils/dependencies.clj)
Functions for building a dependency tree for clojure applications and libraries. Currently limited only to modules/repositories of a single Bitbucket user, as all other libraries are considered as third party.
It uses the Bitbucket api to get the appropriate version of the project.clj file for a module and parses it to get the libraries that the module is dependent on.
Discovered dependency information is saved in ~/.clojure-utils/module-dependency-info.clj, which is read and stored in the utils.dependencies/module-dependency-info-file-path variable, when the module is loaded. You need to 'manually' save the dependency info using the utils.dependencies/save-module-dependency-info function whenever new dependency info is gathered.

### [utils.repositories](https://github.com/fergusonsa/utils-clj/blob/master/utils/src/utils/repositories.clj)
Functions for dealing with git repositories and various functions such as
* cloning missing library repositories
* getting and setting versions of local repositories

## Bugs

Probably lots of bugs and improvements can be made to this. Feel free to make suggestions!
