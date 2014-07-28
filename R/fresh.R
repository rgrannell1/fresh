#!/usr/bin/env Rscript

require(docopt,  quietly = TRUE, warn.conflicts = FALSE)
require(methods, quietly = TRUE, warn.conflicts = FALSE)

import <- source

import('R/main.R',  TRUE)

"
Usage:
    fresh <path> [--verbose]           [report (--simple | --full | --program)] [in range <lower> <upper>]
    fresh <path> <pattern> [--verbose] [report (--simple | --full | --program)] [in range <lower> <upper>]
    fresh (-h | --help | --version)

Description:   Fresh is a command-line tool for gathering statistics on the age
               of a git code-base.

Arguments:
    <path>      The path to the github repository. May be a relative path,
                absolute path, or github path in the form @username/reponame.
    <pattern>   An extended regular expression to determine which
                files should be summarised. Tested against the absolute
                path for each file.
    <lower>     The number.
    <upper>     The number.
Options:
	--version   Show the current version number.
	--verbose   Should fresh show messages aside from the final result?
	--simple    Report the mean age and standard deviation for each file.
	--program   Produce machine-readable output of age data.
" -> doc






args <- docopt(doc)
main(args)
