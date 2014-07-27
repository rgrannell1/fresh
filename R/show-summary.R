
source('R/colourise.R', True)

require(kiwi, quietly = TRUE, warn.conflicts = FALSE)




report <- ( function () {

	self <- list()

	self $ simple <- function (fileStats) {

		width <-
			x_(fileStats)       $
			xMap(x. $ filename) $
			xMap(nchar)         $
			x_MaxBy(xI)

		msg <-
			x_(fileStats) $
			xMap( xUnspread((median : sd : filename) := {

				filename <- gsub(git $ tmppath, '.', filename, fixed = TRUE)

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
	}

	self

})()

# showSummary
#
# Present statistics to the user via the command-line.

showSummary <- (fileStats : projectStats : reporter) := {

	if (reporter == '--simple') {

		report $ simple(fileStats)

	} else if (reporter == '--full') {

	} else if (reporter == '--program') {

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
