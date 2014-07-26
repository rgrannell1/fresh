#!/usr/bin/env Rscript

require(kiwi)
require(git2r)
require(docopt)
require(methods)

"
Usage:
    fresh <path> [-g|--github]
    fresh (-h | --help | --version)

Description: Fresh calculates
    this.

Arguments:
    <path>     The path to the github repository.

Options:
	--github   Should the path be treated as a github repository url
	           of the form 'username/reponame'?
	--version  Show the current version number.

" -> doc

args <- docopt(doc)

if ( xNot(xVersion(), c(0, 38, 0)) ) {
	warning("not written for use with Kiwi > 0.38.0")
}









tempPath <- function (args) {
	tempfile(pattern = "fresh-")
}

git <- ( function () {

	self <- list()

	exec <- function (...) {
		system(paste(...), intern = True)
	}

	toPosix <- function (time, tz) {
		as.POSIXct(as.numeric(time), tz, origin = "1970-01-01")
	}

	self $ abspath <- repoPath := {
		fpath := {
			file.path(repoPath, fpath)
		}
	}

	self $ ls_files <- dpath := {

		x_(exec('cd', dpath, '&&', 'git ls-files')) $
		xMap(self $ abspath(dpath))                 $
		x_Reject(
			xIsMatch('jpg$|png$|jpeg$'))

	}

	self $ blame <- dpath := {
		fpath := {

			# -- get the posix times for each line in a file.

		 	lineDates <-
		 		x_(exec(
		 			'cd', dpath, '&&',
		 			'git blame', fpath, '--line-porcelain')) $
		 		xSelect(
		 			xIsMatch('^author-time|^author-tz') )    $
		 		xMap(xToWords %then% xSecondOf)              $
		 		xChunk(2)                                    $
		 		x_Map(xApply(toPosix) %then% as.numeric)

		 	lineDates
		}
	}

	self $ clone <- userrepo := {

		location <- tempPath()
		clone(paste0('https://github.com/', userrepo, '.git'), location)
		location
	}

	self

} )()





main <- function (args) {

	repoPath <- if (args $ `--github`) {
		git $ clone(args $ `<path>`)
		tempPath()
	} else {
		args $ `<path>`
	}

	print(list.files(repoPath))

	repoFiles_ <- x_(git $ ls_files(repoPath))

	lineDates_ <- repoFiles_ $ xMap(git $ blame(repoPath))

	# -- get the date bounds.
	dateBounds <- list(
		lower = lineDates_ $ xFlatten(1) $ x_MinBy(xI),
		upper = lineDates_ $ xFlatten(1) $ x_MaxBy(xI))

	dateBounds $ diff <- dateBounds $ upper - dateBounds $ lower

	# -- convert dates to percentage of the interval
	# -- between oldest and newest.

	normalised_ <-
		lineDates_ $
		xMap(
			xMap(x. - dateBounds $ lower)) $
		xMap(
			xMap(x. / dateBounds $ diff))

	# -- get the median and standard deviation of the dates
	# -- within each file.

	fileStats <-
		normalised_                          $
		xMap(xAsDouble)                      $
		xMap(
			xJuxtapose_(median, sd))         $
		xZip_(
			repoFiles_ $ x_Identity())       $
		xFlatten(2)                          $
		xMap(
			xJuxtapose_(
				xFirstOf  %then% as.numeric,
				xSecondOf %then% as.numeric,
				xThirdOf))                   $
		xMap(
			xAddKeys(c('median', 'sd', 'filename')) )

	# -- get the median and standard deviation of the dates
	# -- for the entire project.

	projectStats <-
		normalised_                  $
		xFlatten(1)                  $
		xAsDouble()                  $
		xTap(
			xJuxtapose_(median, sd)) $
		x_AddKeys(c('median', 'sd'))

	print(projectStats)

}




main(args)
