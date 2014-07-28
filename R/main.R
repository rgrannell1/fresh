
require(kiwi,  quietly = TRUE, warn.conflicts = FALSE)
require(git2r, quietly = TRUE, warn.conflicts = FALSE)

source('R/git.R',          True)
source('R/path.R',         True)
source('R/show-summary.R', True)

if ( xNot(xVersion(), c(0L, 37L, 0L)) ) {
	warning("not written for use with Kiwi > 0.37.0\n")
}











# getRepoPaths
#
# Get the path from which files are to be read.
# Depends on whether a github repo or local path was specified.

getRepoPath <- args := {

	if (x_(args $ `<path>`) $ x_SliceString(1:4) == 'git:' ) {

		github_repo <- x_(args $ `<path>`) $ x_SliceString(-(1:4))

		git $ clone(github_repo, args $ `--verbose`)
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

	if (xIsEmpty(times)) {
		list()
	} else {
		# -- get the date bounds.

		if ( xNotNull(warnings()) ) {
			warning(warnings()[[1]])
		}

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
			length,
			xThirdOf))                                         $
	xReject(
		(xFirstOf %then% xIsNa) %or% (xSecondOf %then% xIsNa)) $
	xMap(
		xAddKeys(c('median', 'sd', 'obs', 'filename')) )              $
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
		xJuxtapose_(median, sd, length)) $
	x_AddKeys(c('median', 'sd', 'obs'))

}





# validateArgs
#
# Prevent any preventable errors, and
# point out if any inputs cannot be interpreted correctly.

validateArgs <- args := {


}





main <- function (args) {

	validateArgs(args)

	reporter   <- getReporter(args)

	repoPath   <- getRepoPath(args)
	repoFiles  <- getRepoFiles(repoPath, args)

	if (args $ `--verbose`) {
		message('getting dates for each line (slow)')
	}

	linePosix  <- x_(repoFiles) $ x_Map(git $ blame(repoPath))

	if (xIsEmpty(linePosix)) {
		return('')
	}

	normalised <- normalisePosix(linePosix)

	if (args $ `--verbose`) {
		message('summarising each file')
	}

	fileStats  <- getFileStats(repoFiles, normalised)

	if (args $ `--verbose`) {
		message('summarising the project')
	}

	projectStats <- getProjectStats(normalised)

	if (args $ `--verbose`) {
		message('reporting the summary')
	}

	showSummary(fileStats, projectStats, reporter)
}
