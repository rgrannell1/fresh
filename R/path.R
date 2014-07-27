
require(kiwi, quietly = TRUE, warn.conflicts = FALSE)





path <- ( function () {

	self <- list()

	self $ fsep <- .Platform $ file.sep

	self $ join <- (path1 : path2) := {

		if (nchar(path1) == 0) {
			path2
 		} else if (nchar(path2) == 0) {
 			path1
 		} else {

			path1_is_slashed <-
				xSliceString(nchar(path1), path1) == self $ fsep

			path2_is_slashed <-
				xSliceString(1, path2) == self $ fsep

			if (path1_is_slashed && path2_is_slashed) {
				xFromChars_(path1, xSliceString(-1, path2))
			} else if (path1_is_slashed || path2_is_slashed) {
				xFromChars_(path1, path2)
			} else {
				xFromChars_(path1, self $ fsep, path2)
			}

 		}

	}

	self $ components <- xExplode('/')

	self $ basename   <- self $ components %then% xLastOf

	self

} )()
