lazyFibonacci <- function(x, saveState = T)
{
    #first time only - check lf in environment
    if (!exists("lf"))
        lf <- list(lfibo = c(), lnfibo = 0, lnsteps = 0)

    #create core function
    lazyFibonacciCore <- function(x)
    {
        #unpack
        fibo <- lf$lfibo
        nfibo <- lf$lnfibo
        nsteps <- lf$lnsteps

        #calculate
        if (nfibo < 3)
        {
            fibo <- c(1,1)
            nfibo <- 2
            nsteps <- 2
            print("Initializing first two elements: 1, 1")
            #Sys.sleep(2)
        }
        if (nfibo < x)
        {
            for (i in (nfibo + 1):x)
            {
                fibo[i] <- fibo[i - 1] + fibo[i - 2]
                print(paste("Calculating element: ", i))
                #Sys.sleep(1)
            }
            nsteps <- nsteps + (x - nfibo)
            nfibo <- x
        }

        #return
        list(
            store = list(
                lfibo = fibo, lnfibo = nfibo, lnsteps = nsteps
            ), lx = fibo[x]
        )
    }


    #calculate
    ret <- lazyFibonacciCore(x)

    #save
    if (saveState)
        lf <<- ret$store

    #return
    ret$lx
}
