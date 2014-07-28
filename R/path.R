
require(kiwi, quietly = TRUE, warn.conflicts = FALSE)





path <- ( function () {

	self <- list()

	# the operating system specific file seperator.

	self $ fsep <- .Platform $ file.sep

	# join two paths, possibly with /

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

	self $ collapse <- (...) := {
		xImplode(self $ fsep, ...)
	}

	# get each delimited section of a path.

	self $ components <- xExplode(self $ fsep)

	# get the file portion of a path.

	self $ apex_split <- xJuxtapose_(
		xExplode(self $ fsep) %then% xFirstOf,
		xExplode(self $ fsep) %then% xDrop(1) %then% xImplode(self $ fsep)) %then% xAsCharacter

	self

} )()
