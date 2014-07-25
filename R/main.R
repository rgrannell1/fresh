#!/usr/bin/env Rscript

require(kiwi)



asGitPath <- path := {
	file.path(args $ `<path>`, path)
}





git <- ( function () {

	self <- list()

	exec <- function (...) {
		system(paste(...), intern = True)
	}

	toPosix <- function (time, tz) {
		as.POSIXct(as.numeric(time), tz, origin = "1970-01-01")
	}

	self $ ls_files <- dpath := {

		x_(exec('cd', dpath, '&&', 'git ls-files')) $
		xMap(asGitPath)                             $
		x_Reject(
			xIsMatch('jpg$|png$|jpeg$'))

	}

	self $ blame <- fpath := {

		# -- get the posix times for each line in a file
	 	lineDates <-
	 		x_(exec('git blame', fpath, '--line-porcelain')) $
	 		xSelect(
	 			xIsMatch('^author-time|^author-tz') )        $
	 		xMap(xToWords %then% xSecondOf)                  $
	 		xChunk(2)                                        $
	 		x_Map(xApply(toPosix))

		}

	self

} )()






main <- function (args) {

	print('asdasdasdasdasd')

	fpath <- args $ `<path>`
}
