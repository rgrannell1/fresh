
source('R/colourise.R', True)

require(kiwi, quietly = TRUE, warn.conflicts = FALSE)






report <- ( function () {

	self <- list()

	self $ simple <- function (fileStats) {

		path_components <-
			x_(fileStats)            $
			xMap(x. $ filename)      $
			xMap(path $ components)  $
			x_GroupBy(xFirstOf)

		components <-
			x_(fileStats)            $
			xMap(x. $ filename)      $
			x_Map(path $ components)

		fileStats <-
			x_(fileStats)            $
			x_Map(stats := {
				stats $ filename <- path $ components( stats $ filename )
				stats
			})

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

		pad <- (num : string) := {
			strwrap(string, indent = num)
		}

		width <-
			x_(fileStats)                 $
			xMap(x. $ filename)           $
			xMap(xFromChars %then% nchar) $
			x_MaxBy(xI)

		add_tabs <- (li : parent : depth) := {

			if (!is.character(li)) {

				is_directory <-
					xNotEmpty(xSecondOf(li)) &&
					!is.character( xFirstOf(xSecondOf(li)) )

				sep <- if (is_directory) path $ fsep else ''

				name        <- xFirstOf(li)
				padded_name <- pad( depth, paste0(sep, xFirstOf(li)) )

				parent      <- c(parent, name)
				match       <- xSelect(
					xAtKey('filename') %then% xIs(parent), fileStats)

				line <- if (xNotEmpty(match)) {

					median <- xFirstOf(match) $ median
					sd     <- xFirstOf(match) $ sd

					padded_name <- paste(
						gettextf(
							paste0('%-', width, 's'), padded_name), '|',
						colourise $ blue(format(
							round(median, 2), nsmall = 2)),
						'+-',
						colourise $ blue(format(
							round(sd, 2),     nsmall = 2)) )

				} else {
					padded_name
				}

				contents <- x_(li) $ xSecondOf() $ x_Map(elem := {
					add_tabs(elem, parent, depth + 2)
				})

				list(line, contents)
			}
		}

		tree <- add_tabs(
			xFirstOf(group_again(path_components, 2)), c(), 0)

		msg <- x_(tree) $ xFlatten(1) $ xUniqueOf() $ x_FromLines()

		cat(msg, '\n')
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
