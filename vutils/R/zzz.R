# We want to know when loading pkg if:
#   1. Suggested packages are not installed. We avoid Imports to
#      minimize dependences when installing on an HPC.
#   2. A new `vutils` version is available
.onAttach <- function(libname, pkgname) {
    if (!interactive())
        return(NULL)

	if (Sys.info()["sysname"] != "Linux")
		return(NULL)

	parallel::mcparallel({
		dcf <- utils::packageDescription(pkgname)
    	pkg_repo <- dcf$Repository
    	pkg_version <- dcf$Version
    	pkg_suggests <- unlist(strsplit(dcf$Suggests, ","))

    	# Start by checking suggested packages
    	b <- vapply(pkg_suggests, function(pkg) {
    	    name <- trimws(pkg)
    	    length(find.package(name, quiet = T)) == 0
    	}, logical(1))

	    not_installed <- pkg_suggests[b]

	    if (length(not_installed) > 0) {
	        recommended <- paste(not_installed, collapse = ", ")
	        packageStartupMessage(sprintf("The following packages are recommended: %s", recommended))
	    }

	    # Now check if there are any updates available
	    repos <- getOption("repos")
	    if (!pkg_repo %in% names(repos)) {
	        warning(sprintf("Unable to find repository: %s. Defaulting to CRAN.", pkg_repo))
	        pkg_repo <- "CRAN"
	    }

	    cran_url <- repos[names(repos) == pkg_repo]

	    # Default CRAN repo hasn't been set
	    if (cran_url == "@CRAN@")
	        return(NULL)

	    contriburl <- utils::contrib.url(cran_url)

	    cran_version <- NULL
	    if(RCurl::url.exists(contriburl, timeout = 4)) {
			Sys.sleep(3)
	        try(cran_version <- utils::available.packages(contriburl = contriburl)[pkgname, "Version"],
	            silent = T)
		}
	    if (is.null(cran_version)) {
	        packageStartupMessage(sprintf("Unable to determine remote version of %s", pkgname))
	    } else {
	        if (utils::compareVersion(pkg_version, cran_version) < 0)
	            packageStartupMessage(sprintf("[%s] Update available: %s. Currently installed: %s",
	                                          pkgname, cran_version, pkg_version))
	        else
	            packageStartupMessage(sprintf("[%s] is up to date. CRAN version: %s. Currently installed: %s",
                                      pkgname, cran_version, pkg_version))
	    }

    	# auto update if there is a newer version?
    	# update.packages(repos="http://my.local.server/R", ask=FALSE)
	})
 
}
