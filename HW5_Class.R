## HW5 Class/Methods

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)


## set validity method
setValidity(
  "sparse_numeric",
  function(object) {
    # Check that values are numeric
    if (!is.numeric(object@value))
      return("Values must be numeric")
    
    # Check that positions are within valid range
    if (any(object@pos < 1 | object@pos > object@length))
      return("Positions must be within [1, length]")
    
    # Check that number of positions equals number of values
    if (length(object@pos) != length(object@value))
      return("Number of positions must equal number of values")
    
    TRUE
  }
)



## ADD
## Identify a generic function
setGeneric("sparse_add",
           function(x, y, ...) {
             standardGeneric("sparse_add")
           }) 
## Define the method for your class
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...){
            # check same vector length
            if (x@length != y@length)
              stop("Vectors must have the same length")
            
            # convert to regular numeric vectors
            full_x <- as(x, "numeric")
            full_y <- as(y, "numeric")
            
            # perform addition
            as(full_x + full_y, "sparse_numeric")
          })

## MULT
## Identify a generic function
setGeneric("sparse_mult",
           function(x, y, ...) {
             standardGeneric("sparse_mult")
           }) 

## Define the method for your class
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...){
            # check same vector length
            if (x@length != y@length)
              stop("Vectors must have the same length")
            
            # convert to regular numeric vectors
            full_x <- as(x, "numeric")
            full_y <- as(y, "numeric")
            
            # perform addition
            as(full_x * full_y, "sparse_numeric")
          })

## SUB
## Identify a generic function
setGeneric("sparse_sub",
           function(x, y, ...) {
             standardGeneric("sparse_sub")
           }) 
## Define the method for your class
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...){
            # check same vector length
            if (x@length != y@length)
              stop("Vectors must have the same length")
            
            # convert to regular numeric vectors
            full_x <- as(x, "numeric")
            full_y <- as(y, "numeric")
            
            # perform addition
            as(full_x - full_y, "sparse_numeric")
          })

## CROSS PROD
## Identify a generic function
setGeneric("sparse_crossprod",
           function(x, y, ...) {
             standardGeneric("sparse_crossprod")
           }) 
## Define the method for your class
setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...){
            # check same vector length
            if (x@length != y@length)
              stop("Vectors must have the same length")
            
            # convert to regular numeric vectors
            full_x <- as(x, "numeric")
            full_y <- as(y, "numeric")
            
            # perform addition
            sum(full_x * full_y)
          })

## Quick Methods

## Addition
setMethod("+",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

## Subtraction
setMethod("-",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

## Multiplication
setMethod("*",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))


## Coercion Method
## sparse_numeric to numeric
setAs("sparse_numeric", "numeric", function(from) {
  vec <- numeric(from@length)        
  vec[from@pos] <- from@value        
  vec                               
})

## numeric to sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  nonzero <- which(from != 0)
  
  # create object
  new("sparse_numeric",
      value = from[nonzero],
      pos = as.integer(nonzero),
      length = as.integer(length(from)))
})

## show()

setMethod("show", "sparse_numeric", 
          function(object) {
            cat("<sparse_numeric> vector of length", object@length, "\n")
            cat(" Nonzero elements:", length(object@value), "\n")
            # show head of 5 
            if (length(object@value) > 0) {
              preview_n <- min(5, length(object@value))
              cat(" Positions:", paste(head(object@pos, preview_n), collapse = ", "))
              if (length(object@value) > preview_n) cat(", ...")
              cat("\n Values:   ", paste(head(object@value, preview_n), collapse = ", "))
              if (length(object@value) > preview_n) cat(", ...")
              cat("\n")
            }
          })

## plot()

setGeneric("plot")

setMethod(
  "plot",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y, ...) {
    # Check same length
    if (x@length != y@length)
      stop("Vectors must have the same length to plot overlap")
    
    pos_x <- x@pos
    pos_y <- y@pos
    overlap <- intersect(pos_x, pos_y)
    
    # Overlap
    if (length(overlap) == 0) {
      plot(1, type = "n", xlab = "Position", ylab = "", 
           main = "No Overlapping Nonzero Elements", 
           xlim = c(1, x@length), ylim = c(0, 1))
      text(x@length / 2, 0.5, "No overlap found", col = "gray40")
      return(invisible(NULL))
    }
    
    # Plot overlaps
    plot(
      overlap, rep(1, length(overlap)),
      pch = 16, col = "darkslategrey",
      xlim = c(1, x@length),
      ylim = c(0.5, 1.5),
      xlab = "Position (index)",
      ylab = "",
      yaxt = "n",
      main = "Overlapping Nonzero Elements in Sparse Vectors",
      ...
    )
    
    axis(2, at = 1, labels = "overlap", las = 1)
  }
)


## length()
setGeneric("length")

setMethod(
  "length",
  # return length to user
  signature(x = "sparse_numeric"),
  function(x) {
    x@length
  }
)
