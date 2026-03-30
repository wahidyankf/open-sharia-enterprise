package cmd

import (
	"github.com/wahidyankf/open-sharia-enterprise/libs/hugo-commons/links"
)

// checkLinksFn is a package-level variable for dependency injection in tests.
var checkLinksFn = links.CheckLinks
