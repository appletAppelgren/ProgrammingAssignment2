makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
         list(set = set, get = get,
            setmean = setmean,
            getmean = getmean)
        #print(x)
        #print(ll)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        ##m <- getElement(x, getmean)()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

r = rnorm(1000000)
class(r)
#mat1 = matrix(r, nrow=1000, ncol=1000)
t<-makeVector(r)
cachemean(t)
