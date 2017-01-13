makeCacheMatrix <- function(x = matrix(c(1,0,0,1), nrow=2, ncol=2)) {
        #Functie om  de inverse van een matrix te cachen
        
        # Bij de eerste keer aanroepen maken we een lege variabele voor de 
        # inverse
        inv <- NULL
        # Als we set aanroepen vullen we de matrix via set-functie
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # De get functie geeft de waarde x terug
        get <- function() x
        # Dan de functie om de inverse te setten
        setinv <- function(solve) inv <<- solve
        # En op te halen
        getinv <- function() inv
        # Tot slot geven we een lijst van functies terug die we op de matrix
        # en zijn inverse kunnen aanroepen
        list(set = set, get = get, setinv = setinv, getinv=getinv)        
}

cacheSolve <- function(x, ...) {
        # Functie om de gecachete inverse van een matrix op te halen
        
        # Probeer eerst de inverse op te halen
        inv <- x$getinv()
        # Als deze al bestaat, zijn we klaar en geven we de inverse terug
        if(!is.null(inv)) {
                message("Getting cached inverse")
                return(inv)
        }
        # In andere gevallen gaan we de inverse uitrekenen en opslaan voor
        # toekomstig gebruik
        # Haal de matrix op
        data <- x$get()
        # Bereken de inverse
        inv <- solve(data, ...)
        # Sla het resultaat op in de cachematrix
        x$setinv(inv)
        # Toon de inverse als output
        inv
}
