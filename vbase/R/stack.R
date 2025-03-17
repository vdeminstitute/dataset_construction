#' Wrapper for \code{sys.status()}
stackStatus <- function() {
    sys.status()
}

#' Return the frame number of the calling function
#' 
#' @return The frame number of the calling function
#' @export
callerFrameNumber <- function() {
    
    status <- stackStatus()
    innerFrame <- sys.nframe()
    if (innerFrame == 0) stop("Inner frame is global environment")

    callerFrame <- innerFrame - 1
    if (callerFrame == 0) return(0)
    if (callerFrame < 0) stop("Negative frame number")

    parentFrameCaller <- status[["sys.parents"]][callerFrame]
    res <- parentFrameCaller

    return(res)
}

#' Return the environment of the calling function
#' 
#' @return The environment of the calling function
#' @export
callerEnvironment <- function() {

    status <- stackStatus()
    innerFrame <- sys.nframe()
    if (innerFrame == 0) stop("Inner frame is global environment")

    callerFrame <- innerFrame - 1
    if (callerFrame == 0) return(0)
    if (callerFrame < 0) stop("Negative frame number")

    parentFrameCaller <- status[["sys.parents"]][callerFrame]
    res <- sys.frame(parentFrameCaller)
    if (!is.environment(res)) stop("Not an environment")

    return(res)
}

#' Return the call of the calling function
#' 
#' @param getName If \code{TRUE}, return the name of the function
#' @return The call of the calling function
#' @export
callerCall <- function(getName = FALSE) {

    status <- stackStatus()
    innerFrame <- sys.nframe()
    if (innerFrame == 0) stop("Inner frame is global environment")

    callerFrame <- innerFrame - 1
    if (callerFrame == 0) return(0)
    if (callerFrame < 0) stop("Negative frame number")

    res <- sys.call(callerFrame)
    if (!is.call(res)) stop("Not a call")

    if (getName) {
        res <- deparse(res[[1]])
        if (!is.function(get(res))) stop("Not a function")
    }

    return(res)
}

#
contextFrameNumber <- function(n = 1) {

    n <- n + 1
    status <- stackStatus()
    if (n > length(status[["sys.calls"]])) stop("Context is too deep, not enough frames")
    innerFrame <- sys.nframe()
    if (innerFrame == 0) stop("Inner frame is global environment")

    contextFrame <- innerFrame - n
    if (contextFrame == 0) return(0)
    if (contextFrame < 0) stop("Negative frame number")

    contextCaller <- status[["sys.parents"]][contextFrame]
    res <- contextCaller

    return(res)
}

#
contextEnvironment <- function(n = 1) {

    n <- n + 1
    status <- stackStatus()
    innerFrame <- sys.nframe()
    if (innerFrame == 0) stop("Inner frame is global environment")

    contextFrame <- innerFrame - n
    if (contextFrame == 0) return(globalenv())
    if (contextFrame < 0) stop("Negative frame number")

    contextCaller <- status[["sys.parents"]][contextFrame]
    res <- sys.frame(contextCaller)
    if (!is.environment(res)) stop("Not an environment")

    return(res)
}

# 
contextCall <- function(n = 1, getName = FALSE) {

    n <- n + 1
    status <- stackStatus()
    innerFrame <- sys.nframe()
    if (innerFrame == 0) stop("Inner frame is global environment")

    contextFrame <- innerFrame - n
    if (contextFrame == 0) return(NULL)
    if (contextFrame < 0) stop("Negative frame number")

    res <- sys.call(contextFrame)
    if (!is.call(res)) stop("Not a call")

    if (getName) {
        res <- deparse(res[[1]])
        if (!is.function(get(res))) stop("Not a function")
    }

    return(res)
}