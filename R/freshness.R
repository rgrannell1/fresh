#!/usr/bin/env Rscript

require(kiwi)
require(docopt)
require(methods)

"
Usage:
    fresh <path>
    fresh (-h | --help | --version)

Description: Fresh calculates
    this.

Arguments:
    <path>     The path to the github repository.

Options:
	--version  Show the current version number.

" -> doc

args <- docopt(doc)

if ( xNot(xVersion(), c(0, 38, 0)) ) {
	warning("not written for use with Kiwi > 0.38.0")
}

main(args)
