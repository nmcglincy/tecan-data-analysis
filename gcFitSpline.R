function (time, data, gcID = "undefined", control = grofit.control()) 
{
    if (is(control) != "grofit.control") 
        stop("control must be of class grofit.control!")
    if (!control$fit.opt %in% c("s", "b")) 
        stop("Fit option is not set for a spline fit. See grofit.control()")
    time <- as.vector(as.numeric(as.matrix(time)))
    data <- as.vector(as.numeric(as.matrix(data)))
    if (length(time) != length(data)) 
        stop("gcFitSpline: length of input vectors differ!")
    bad.values <- (is.na(time)) | (time < 0) | (is.na(data)) | 
        (data < 0) | (!is.numeric(time)) | (!is.numeric(data))
    if (TRUE %in% bad.values) {
        if (control$neg.nan.act == FALSE) {
            time <- time[!bad.values]
            data <- data[!bad.values]
        }
        else {
            stop("Bad values in gcFitSpline")
        }
    }
    if (length(data) < 5) {
        cat("gcFitSpline: There is not enough valid data. Must have at least 5!")
        gcFitSpline <- list(raw.time = time, raw.data = data, 
            gcID = gcID, fit.time = NA, fit.data = NA, parameters = list(A = NA, 
                mu = NA, lambda = NA, integral = NA), parametersLowess = list(A = NA, 
                mu = NA, lambda = NA), spline = NA, reliable = NULL, 
            fitFlag = FALSE, control = control)
        class(gcFitSpline) <- "gcFitSpline"
        return(gcFitSpline)
    }
    else {
        if (control$log.x.gc == TRUE) {
            time <- log(1 + time)
        }
        if (control$log.y.gc == TRUE) {
            data <- log(1 + data)
        }
        halftime <- (min(time) + max(time))/2
        try(y.spl <- smooth.spline(time, data, spar = control$smooth.gc))
        if (is.null(y.spl) == TRUE) {
            warning("Spline could not be fitted to data!")
            if (is.null(control$smooth.gc) == TRUE) {
                cat("This might be caused by usage of smoothing parameter NULL\n")
                fit.nonpara <- list(raw.x = time, raw.y = data, 
                  fit.x = NA, fit.y = NA, parameters = list(A = NA, 
                    mu = NA, lambda = NA, integral = NA), spline = NA, 
                  parametersLowess = list(A = NA, mu = NA, lambda = NA), 
                  spline = NA, reliable = NULL, fitFlag = FALSE, 
                  control = control)
                class(gcFitSpline) <- "gcFitSpline"
                return(gcFitSpline)
            }
        }
        dydt.spl <- predict(y.spl, time, deriv = 1)
        index <- which.max(dydt.spl$y)
        t.max <- dydt.spl$x[index]
        dydt.max <- max(dydt.spl$y)
        y.max <- y.spl$y[index]
        mu.spl <- dydt.max
        b.spl <- y.max - dydt.max * t.max
        lambda.spl <- -b.spl/mu.spl
        integral <- low.integrate(y.spl$x, y.spl$y)
        low <- lowess(time, data, f = 0.25)
        y.low <- low$y
        x.low <- low$x
        dydt.low <- diff(y.low)/diff(time)
        mu.low <- max(dydt.low)
        index <- which.max(dydt.low)
        t.max <- x.low[index]
        y.max <- y.low[index]
        b.low <- y.max - mu.low * t.max
        lambda.low <- (-1) * b.low/mu.low
    }
    gcFitSpline <- list(raw.time = time, raw.data = data, gcID = gcID, 
        fit.time = y.spl$x, fit.data = y.spl$y, parameters = list(A = max(y.spl$y), 
            mu = mu.spl, lambda = lambda.spl, integral = integral), 
        parametersLowess = list(A = max(y.low), mu = mu.low, 
            lambda = lambda.low), spline = y.spl, reliable = NULL, 
        fitFlag = TRUE, control = control)
    class(gcFitSpline) <- "gcFitSpline"
    gcFitSpline
}
<environment: namespace:grofit>