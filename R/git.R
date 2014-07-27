
require(kiwi, quietly = TRUE, warn.conflicts = FALSE)





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

		if (!file.exists(dpath)) {
			list(0)
		} else {
			x_(exec('cd', dpath, '&&', 'git ls-files')) $
			xMap(self $ abspath(dpath))                 $
			x_Reject(
				xIsMatch('jpg$|png$|jpeg$'))
		}
	}

	# git $ blame
	#
	# Get the POSIX time associated with each line in a file
	# as a number, adjusted for time zone.

	self $ blame <- dpath := {
		fpath := {

			if (!file.exists(dpath)) {
				list()
			} else {
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
