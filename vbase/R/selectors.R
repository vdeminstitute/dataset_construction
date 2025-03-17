substituteContextSymbols <- function(expr, context) {
    
    inLength <- length(expr)
    fExpr <- substitute(expr, list(expr = expr))
    subExpr <- eval(substitute(substitute(fExpr, context), list(fExpr = fExpr)))
    stopifnot(length(subExpr) == inLength)
    
    return(subExpr)
}

evalDataFrame <- function(dataFrame, expr, includeNA) {
    
    stopifnot(is.data.frame(dataFrame))
    stopifnot("list" %in% class(expr))

    out <- vapply(
            X = expr,
            FUN = function(i, env) {
                eval(i, envir = env, enclos = emptyenv())
                },
            FUN.VALUE = logical(nrow(dataFrame)),
            env = list2env(dataFrame))
    stopifnot(is.matrix(out) && ncol(out) == length(expr) && nrow(out) == nrow(dataFrame))

    if (anyNA(out)) {

        if (includeNA) {
            f <- function(x, y) { any(x, y) }
        } else {
            f <- function(x, y) { isTRUE(x)  }
        }

        for (i in seq(from = 1, to = nrow(out))) {
            for (j in seq(from = 1, to = ncol(out))) {
                out[i, j] <- f(x = out[i, j], y = is.na(out)[i, j])
            }
        }
    }

    return(out)
}

evalContext <- function(dots, context) {

    contextCandidates <- vapply(
        X = dots,
        FUN = deparse, 
        FUN.VALUE = character(1))

    # TODO: Add support for global environment?
    candidatesExists <-
        vapply(
            X = contextCandidates,
            FUN = function(i) { exists(i, mode = "character", envir = context) },
            FUN.VALUE = logical(1))

    if (isTRUE(all(candidatesExists))) {
        out <- lapply(
            X = dots,
            FUN = function(i) { eval(i, envir = context, enclos = emptyenv()) })

        outClean <- as.list(unique(unlist(out)))
        return(outClean)
    }

    return(dots)
}

makeSymbols <- function(dots) {
    
    # Unlist if only symbols
    if (isTRUE(all(vapply(dots, is.symbol, logical(1))))) {
        out <- unlist(dots)

    # Unlist if only characters
    } else if (isTRUE(all(vapply(dots, is.character, logical(1))))) {
        out <- sapply(dots, as.symbol)

    # If c() holds symbols/characters
    } else if (isTRUE(deparse(dots[[1]][[1]]) == "c" )) {

        # Drop c()
        res <- cDrop(dots[[1]])

            # Symbols we keep as is
            # Make characters into symbols        
            if (isTRUE(all(vapply(res, is.character, logical(1))))) {
                res <- sapply(res, as.symbol)
            }
        
        stopifnot(isTRUE(all(vapply(res, is.symbol, logical(1)))))
        out <- res

    } else {
        err <- sprintf("Cannot resolve input to %s", callerCall(getName = TRUE))
        stop(err)
    }

    return(out)
}

cDrop <- function(cCall) {
    
    cCall[[1]] <- NULL
    res <- Filter(f = function(i) { !is.null(i) }, x = cCall)

    return(res)
}

makePick <- function(symList) {

    out <- vapply(X = symList, FUN = deparse, FUN.VALUE = character(1))
    return(out)
}

pruneAny <- function(pickVec, env) {

    df <- NULL
    dfNames <- names(bquote(.(df), env))
    out <- intersect(x = pickVec, y = dfNames)

    if (is.null(out)) stop("pruneAny produces a NULL")
    if (!length(out) > 0) stop("pruneAny results in a zero length vector")
    return(out)
}

evalPick <- function(pickVec, env) {
    
    df <- NULL
    out <- bquote(.(df), where = env)[, pickVec, drop = FALSE]
    stopifnot(all(pickVec %in% names(out)) && ncol(out) == length(pickVec))
    
    return(out)
}


#' @title Ensure to pick all variables
#' @description Ensure to pick all selected variables
#' @param ... Variables to pick, either as symbol or character.
#' @return A data frame with the selected variables.
#' 
#' @examples 
#' df <- data.frame(a = 1:10, b = 1:10, c = 1:10)
#' selectColumns(df, pickAll(a, b)) # select a and b
#' selectColumns(df, pickAll(A, b, c)) # fail as A is not in df
#' 
#' @export
pickAll <- function(...) {
    
    generationsBack <- 3
    contextCall <- contextCall(n = generationsBack, getName = TRUE)
    if (!isTRUE(contextCall == "selectColumns")) {
        stop("Should be called selectColumns")
    }

    # Capture dots
    .dots <- eval(substitute(alist(...)))
    stopifnot(`pickAll has no input` = length(.dots) > 0)
    
    # Frames
    selectEnv <- contextEnvironment(n = generationsBack - 1)
    contextEnv <- contextEnvironment(n = generationsBack)

    # First evaluate contextual objects and then make a symList
    symList <- makeSymbols(evalContext(.dots, contextEnv))
    # Create selector vector
    res <- makePick(symList)

    stopifnot(`pickAll cannot find all columns` =
        all(res %in% names(bquote(.(df), selectEnv))))

    out <- evalPick(res, selectEnv)
    return(out)
}

#' @title Try to pick any variables
#' @description Try to pick any selected variables
#' @param ... Variables to pick, either as symbol or character.
#' @return A data frame with the selected variables.
#' 
#' @examples
#' df <- data.frame(a = 1:10, b = 1:10, c = 1:10)
#' selectColumns(df, pickAny(a, b)) # select a and b
#' selectColumns(df, pickAny(A, b, c)) # select b and c
#' 
#' @export
pickAny <- function(...) {

    generationsBack <- 3
    contextCall <- contextCall(n = generationsBack, getName = TRUE)
    if (!isTRUE(contextCall == "selectColumns")) {
        stop("Should be called selectColumns")
    }

    # Capture dots
    .dots <- eval(substitute(alist(...)))
    stopifnot(`pickAny has no input` = length(.dots) > 0)
    
    # Frames
    selectEnv <- contextEnvironment(n = generationsBack - 1)
    contextEnv <- contextEnvironment(n = generationsBack)

    # First evaluate contextual objects and then make a symList
    symList <- makeSymbols(evalContext(.dots, contextEnv))
    # Create selector vector
    picked <- makePick(symList)
    # Prune selector vector
    res <- pruneAny(picked, selectEnv)

    stopifnot(`pickAny cannot find any column` = 
        any(res %in% names(bquote(.(df), selectEnv))))

    out <- evalPick(res, selectEnv)
    return(out)
}