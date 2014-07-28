
source('R/colourise.R', True)

require(kiwi, quietly = TRUE, warn.conflicts = FALSE)






report <- ( function () {

	self <- list()

	self $ simple <- function (fileStats) {

		path_components <-
			x_(fileStats)            $
			xMap(x. $ filename)      $
			xMap(path $ components)  $
			xGroupBy(xFirstOf)

		group_again <- (grouped : ith) := {

			x_(grouped) $ x_Map( xUnspread((shared : members) := {

				if (x_(members) $ x_LenOf() < ith) {
					list(shared, members)
				} else {

					list(
						shared,
						group_again(
							xGroupBy(xAt(ith), members), ith + 1)
					)
				}

			}) )

		}




		path_tree <- xFirstOf(group_again(path_components, 2))

		pad <- (num : string) := {
			strwrap(string, indent = num)
		}

		add_tabs <- (li : parent : depth) := {

			if (is.character(li)) {
				if ( xNot(li, gsub('^[ ]+', '', li)) ) {
					pad(depth, xLastOf(li))
				}
			} else {

				name     <- pad(depth, xFirstOf(li))
				contents <- x_(xSecondOf(li)) $ xMap(elem := {
					add_tabs(elem, name, depth + 2)
				})

				list(name, contents)
			}
		}

		tree <- add_tabs(path_tree, '', 0)

		aa = x_(tree) $ xFlatten(1) $ xUniqueOf() $ x_FromLines()

		cat(aa, '\n')


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
