#!/usr/bin/env Rscript

require(kiwi,    quietly = TRUE, warn.conflicts = FALSE)
require(git2r,   quietly = TRUE, warn.conflicts = FALSE)
require(docopt,  quietly = TRUE, warn.conflicts = FALSE)
require(methods, quietly = TRUE, warn.conflicts = FALSE)

"
Usage:
    fresh <path> [-g|--github] [--verbose] [report (--simple | --full | --program)]
    fresh <path> <pattern>     [--github] [--verbose] [report (--simple | --full | --program)]
    fresh (-h | --help | --version)

Description:   Fresh is a command-line tool for gathering statistics on the age
               of a git code-base.

Arguments:
    <path>      The path to the github repository. May be a relative path,
                absolute path, or github path in the form username/reponame.
    <pattern>   An extended regular expression to determine which
                files should be summarised. Tested against the absolute
                path for each file.
Options:
	--github    Should the path be treated as a github repository url
	            of the form 'username/reponame'?
	--version   Show the current version number.
	--verbose   Should fresh show messages aside from the final result?
" -> doc






args <- docopt(doc)

if ( xNot(xVersion(), c(0L, 37L, 0L)) ) {
	warning("not written for use with Kiwi > 0.37.0\n")
}











git <- ( function () {

	self <- list()

	# exec
	#
	# Call system, intern the results.

	exec <- function (...) {
		system(paste(...), intern = True)
	}

	# toPosix
	#
	# Convert a number timezone pair to posix object.

	toPosix <- function (time, tz) {
		as.POSIXct(as.numeric(time), tz, origin = "1970-01-01")
	}

	# git $ tmppath
	#
	# The path that cloned repos are written to.

	self $ tmppath <- tempfile(pattern = "fresh-")

	# git $ abspath
	#
	# Append the repository path to a relative file path.

	self $ abspath <- repoPath := {
		fpath := {
			file.path(repoPath, fpath)
		}
	}

	# git $ ls_files
	#
	# List all files that have editable lines, using git ls-files.
	# Filters out erroneous inputs like images.

	self $ ls_files <- dpath := {

		x_(exec('cd', dpath, '&&', 'git ls-files')) $
		xMap(self $ abspath(dpath))                 $
		x_Reject(
			xIsMatch('jpg$|png$|jpeg$'))

	}

	# git $ blame
	#
	# Get the POSIX time associated with each line in a file
	# as a number, adjusted for time zone.

	self $ blame <- dpath := {
		fpath := {

	 		x_(exec(
	 			'cd', dpath, '&&',
	 			'git blame', fpath, '--line-porcelain')) $
	 		xSelect(
	 			xIsMatch('^author-time|^author-tz') )    $
	 		xMap(xToWords %then% xSecondOf)              $
	 		xChunk(2)                                    $
	 		x_Map(xApply(toPosix) %then% as.numeric)

		}
	}

	# git $ clone
	#
	# Clone a repository from github into a temporary location.
	# Uses git2R for now; will re-implement with base R.

	self $ clone <- (userrepo : isVerbose) := {

		location <- self $ tmppath
		dir.create(location)
		clone(paste0('https://github.com/', userrepo, '.git'), location, isVerbose)
		cat('\n')
		location
	}

	self

} )()


# getRepoPath
#
# Get the path from which files are to be read.
# Depends on whether a github repo or local path was specified.

getRepoPath <- args := {

	if (args $ `--github`) {
		git $ clone(args $ `<path>`, args $ `--verbose`)
		git $ tmppath
	} else {
		args $ `<path>`
	}
}



# getRepoFiles
#
# Return the files to check the blame of; if a pattern is given
# some files are filtered out.

getRepoFiles <- (path : args) := {

	x_(git $ ls_files(path)) $
	x_Select(
		if (xIsNull(args $ `<pattern>`)) {
			xTruth
		} else {
			xIsMatch(args $ `<pattern>`)
		}
	)

}



# normalisePosix
#
# Takes a list of UNIX time numbers. Finds the oldest and newest numbers, and
# sets the oldest to 0 on a scale and newest to 1. Maps every other UNIX time number
# to a point on this interval.

normalisePosix <- times := {

	# -- get the date bounds.
	dateBounds <- list(
		lower = x_(times) $ xFlatten(1) $ x_MinBy(xI),
		upper = x_(times) $ xFlatten(1) $ x_MaxBy(xI))

	dateBounds $ diff <- dateBounds $ upper - dateBounds $ lower

	# -- convert dates to percentage of the interval
	# -- between oldest and newest.

	# -- the oldest line = 0
	# -- the newest line = 1

	x_(times) $
		xMap(
			xMap(x. - dateBounds $ lower)) $
		x_Map(
			xMap(x. / dateBounds $ diff))
}



# getFileStats
#
# Get the median age (0 = oldest line in library, 1 = newest line in library)
# of each line within a file, categorise by file, and sorted.
# Remove empty files.

getFileStats <- (repoFiles : percents) := {

	x_(percents)                                               $
	xMap(xAsDouble)                                            $
	xMap(
		xJuxtapose_(median, sd))                               $
	xZip_(
		repoFiles)                                             $
	xFlatten(2)                                                $
	xMap(
		xJuxtapose_(
			xFirstOf  %then% as.numeric,
			xSecondOf %then% as.numeric,
			xThirdOf))                                         $
	xReject(
		(xFirstOf %then% xIsNa) %or% (xSecondOf %then% xIsNa)) $
	xMap(
		xAddKeys(c('median', 'sd', 'filename')) )              $
	x_SortBy(x. $ median)

}


# getProjectStats
#
# Get the overall age and variance in age of lines within a
# library.

getProjectStats <- percents := {

	x_(percents)                 $
	xFlatten(1)                  $
	xAsDouble()                  $
	xTap(
		xJuxtapose_(median, sd)) $
	x_AddKeys(c('median', 'sd'))

}



# showSummary
#
# Present statistics to the user via the command-line.

showSummary <- (fileStats : projectStats : reporter) := {

	if (reporter == '--simple') {

		width <-
			x_(fileStats)       $
			xMap(x. $ filename) $
			xMap(nchar)         $
			x_MaxBy(xI)

		colourise <- kiwi ::: colourise

		msg <-
			x_(fileStats) $
			xMap( xUnspread((median : sd : filename) := {

				print('asd')

				filename <- gsub(git $ tmppath, '.', row $ filename, fixed = TRUE)

				paste(
					gettextf(
						paste0('%-', width, 's'), filename), '|',
					colourise $ blue(format(
						round(median, 2), nsmall = 2)),
					'+-',
					colourise $ blue(format(
						round(sd, 2),     nsmall = 2)) )


			}) )          $
			x_FromLines()

		message(msg)

	} else if (reporter == '--full') {

	}else if (reporter == '--program') {

		msg <-
			x_(fileStats)                 $
			xMap(row := {

				row $ filename <-
					gsub(git $ tmppath, '.', row $ filename, fixed = TRUE)
				row

			})                            $
			xFlatten(1)                   $
			xChunk(3)                     $
			xMap(paste %then% xFromLines) $
			x_Implode('\n\n')

		message(msg)

	}

}



# validateArgs
#
# Prevent any preventable errors, and
# point out if any inputs cannot be interpreted correctly.

validateArgs <- args := {

}




# getReporter
#
# Get the mode by which data will be displayed.
#
#

getReporter <- function (args) {
	if (args $ report) {

		x__('--simple', '--full', '--program') $
		xSelect(
			flag := xIsTrue( args [[flag]] ))  $
		x_AsCharacter()

	} else {
		'--simple'
	}
}





main <- function (args) {

	validateArgs(args)

	reporter   <- getReporter(args)

	repoPath   <- getRepoPath(args)
	repoFiles  <- getRepoFiles(repoPath, args)

	if (args $ `--verbose`) {
		message('getting dates for each line (slow)')
	}

	linePosix  <- x_(repoFiles) $ xMap(git $ blame(repoPath))
	normalised <- normalisePosix(linePosix)

	if (args $ `--verbose`) {
		message('summarising each file')
	}

	fileStats    <- getFileStats(repoFiles, normalised)

	if (args $ `--verbose`) {
		message('summarising the project')
	}

	projectStats <- getProjectStats(normalised)

	showSummary(fileStats, projectStats, reporter)
}




main(args)
