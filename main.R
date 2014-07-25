#!/usr/bin/env Rscript

require(kiwi)

exec <- function (...) {
	system(paste(...), intern = True)
}


gitLsFiles <- path := {

	asGitPath <- relpath := {
		file.path(repoPath, relpath)
	}

	x_(exec('cd', path, '&&', 'git ls-files')) $
	xMap(asGitPath) $
	xReject(xIsMatch('png$|jpg$|jpeg$'))
}

toPosix <- function (time, tz) {
	as.POSIXct(as.numeric(time), tz, origin = "1970-01-01")
}

gitBlame <- fpath := {

	lineDates <-
		x_(exec('git blame', fpath, '--line-porcelain')) $
		xSelect(
			xIsMatch('^author-time|author-tz') )         $
		xMap(xToWords %then% xSecondOf)                  $
		xChunk(2)                                        $
		x_Map(xApply(toPosix))


	list(fpath = fpath, lineDates = lineDates)
}

lineDates <- x_(gitLsFiles(repoPath)) $ xMap(gitBlame)
