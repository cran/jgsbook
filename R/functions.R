####################################################
# In order to let roxygen2() create all man pages, #
# you must run the following command:              #
#       roxygen2::roxygenise()                     #
####################################################


#' create a frequency table
#'
#' returns a frequency table with absolute and relative frequencies and cumulated frequencies
#' @param werte factor with obeserved data
#'
#' @return dataframe table
#'
#' @examples
#' x <- ceiling(stats::rnorm(20))
#' freqTable(x)
#'
#' @export
#---------------------------------------------------------------
freqTable <- function(werte){
  x <- table(werte)
  tabelle <- data.frame(x)
  tabelle$freqcum <- cumsum(x)
  tabelle$relfreq <- round(x/length(werte)*100,2)
  tabelle$relcum  <- cumsum(round(x/length(werte)*100,2))
  colnames(tabelle) <- c("Wert", "Haeufig", "Hkum", "Relativ", "Rkum")
  #tabelle$Wert <- as.numeric(as.vector(tabelle$Wert))
  return(tabelle)
}
#---------------------------------------------------------------


#' create a tibble with kenngroessen
#'
#' returns a tibble with all kenngroessen
#' @param werte numeric vector
#'
#' @return tibble with all kenngroessen
#'
#' @import statip
#' @importFrom stats IQR median qnorm qt quantile sd var
#' @examples
#' x <- ceiling(stats::rnorm(20))
#' kenngroessen(x)
#'
#' @export
#---------------------------------------------------------------
kenngroessen <- function(werte){
  bla <- data.frame(0)
  bla$modus=paste(as.character(statip::mfv(werte)), collapse="|")
  bla$mean=mean(werte, na.rm=T)
  bla$median=median(werte, na.rm=T)
  bla$p25=quantile(werte,0.25,type=6)
  bla$p75=quantile(werte,0.75,type=6)
  bla$iqr=IQR(werte,type=6)
  bla$sd=sd(werte, na.rm=T)
  bla$var=var(werte, na.rm=T)
  bla$VK= (sd(werte, na.rm=T)/mean(werte,na.rm=T))
  return(bla[-1])
}
#---------------------------------------------------------------




#' compute confidence intervall  for binomial proportions
#'
#' returns borders and length of confidence intervall for binomial proportions
#' @param p proportion obeserved
#'
#' @param n number of observations
#'
#' @param alpha error niveau
#'
#' @return confidence intervall
#'
#' @examples
#' KIbinomial_a(0.35, 150, 0.05)
#'
#' @export
#---------------------------------------------------------------
KIbinomial_a <- function(p, n, alpha){
  fehler <- qnorm(1-alpha/2)*sqrt(p*(1-p)/n)
  x <- c(paste(1-alpha, "KI untere Grenze"),
         paste(1-alpha, "KI obere Grenze"),
         paste(1-alpha, "KI Laenge"))
  y <- c(p-fehler, p+fehler, 2*fehler)
  KI <- data.frame(x,y)
  return(KI)
}
#---------------------------------------------------------------


#' compute confidence intervall for difference of binomial proportions
#'
#' returns borders and length of confidence intervall for difference of binomial proportions
#' @param p1 proportion obeserved in group 1
#'
#' @param p2 proportion obeserved in group 2
#'
#' @param n1 number of observations in group 1
#'
#' @param n2 number of observations in group 2
#'
#' @param alpha error niveau
#'
#' @return confidence intervall
#'
#' @examples
#' KIbinomial_u(0.25, 100, 0.4, 150, 0.05)
#'
#' @export
#---------------------------------------------------------------
KIbinomial_u <- function(p1, n1, p2, n2, alpha){
  d <- p2-p1
  fehler <- qnorm(1-alpha/2)*sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
  x <- c("Differenz der Anteile",
         paste(1-alpha, "KI untere Grenze"),
         paste(1-alpha, "KI obere Grenze"),
         paste(1-alpha, "KI Laenge"))
  y <- c(d, d-fehler, d+fehler, 2*fehler)
  KI <- data.frame(x,y)
  return(KI)
  }
#---------------------------------------------------------------


#' compute confidence intervall  for mean of normal distributed data
#'
#' returns borders and length of confidence intervall for mean of normal distributed data
#' @param xquer mean of obeserved data
#'
#' @param s standard deviation of observed data
#'
#' @param n number of observations
#'
#' @param alpha error niveau
#'
#' @return confidence intervall
#'
#' @examples
#' KInormal_a(400, 20, 100, 0.05)
#'
#' @export
#---------------------------------------------------------------
KInormal_a <- function(xquer, s, n, alpha){
  fehler <- qt(1-alpha/2, df=n-1)*s/sqrt(n)
  x <- c(paste(1-alpha, "KI untere Grenze"),
         paste(1-alpha, "KI obere Grenze"),
         paste(1-alpha, "KI Laenge"))
  y <- c(xquer-fehler, xquer+fehler, 2*fehler)
  KI <- data.frame(x,y)
  return(KI)
}
#---------------------------------------------------------------



#' compute confidence intervall  for mean of normal distributed data
#'
#' returns a data.frame with borders and length of confidence intervall for mean of normal distributed data
#' @param x1 mean of obeserved data in group 1
#'
#' @param s1 standard deviation of observed data in group 1
#'
#' @param n1 number of observations in group 1
#'
#' @param x2 mean of obeserved data in group 2
#'
#' @param s2 standard deviation of observed data in group 2
#'
#' @param n2 number of observations in group 2
#'
#' @param alpha error niveau
#'
#' @return data.frame of confidence intervall
#'
#' @examples
#' KInormal_u(2.22, 0.255, 13, 2.7, 0.306, 10 , 0.05)
#'
#' @export
#---------------------------------------------------------------
KInormal_u <- function(x1, s1, n1, x2, s2, n2, alpha){
  d <- x2-x1
  s_pool = ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2)
  fehler <- qt(1-alpha/2,df=n1+n2-1)*sqrt(s_pool^2/n1 + s_pool^2/n2)
  x <- c("Differenz der Mittelwerte",
         paste(1-alpha, "KI untere Grenze"),
         paste(1-alpha, "KI obere Grenze"),
         paste(1-alpha, "KI Laenge"))
  y <- c(d, d-fehler, d+fehler, 2*fehler)
  KI <- data.frame(x,y)
  return(KI)
}
#---------------------------------------------------------------


#' compute sensitivity and specifity
#'
#' returns sensitivity specifity, negativ-predictive-value, postitiv-predictive-value
#' @param rp number of true-positive (richtig-positiv)
#'
#' @param rn number of true-negative (richtig-negativ)
#'
#' @param fp number of false-positive (falsch-positiv)
#'
#' @param fn number of false-negative (falsch-negativ)
#'
#' @return a data.frame with sens, spec, ppw, npw
#'
#' @examples
#' sens.spec(40, 17, 85, 4)
#'
#' @export
sens.spec <- function(rp, rn, fp, fn){
  x <- data.frame(
    sens=round(rp/(rp+fn)*100, 2),
    spec=round(rn/(rn+fp)*100, 2),
    ppw =round(rp/(rp+fp)*100, 2),
    npw =round(rn/(rn+fn)*100, 2)
  )
  return(x)
}
#---------------------------------------------------------------

#' z-Transformation by given numbers, with
#' z = (x - mu) / sd
#' @param x a value to transform
#' @param mu the given mu
#' @param sd the given standard deviation
#' @return the z-transformed value
#' @examples
#' ztrans(120,mu=118,sd=20)
#'
#' @export
ztrans <- function(x, mu=0, sd=1){
  z = (x-mu)/sd
  return(z)
}

#' get longitude and altitude from an address
#' using OpenStreetMap's API at
#' http://nominatim.openstreetmap.org
#' @param address a character of an address
#' @return a data.frame containig "address", "lon", "lat"
#' @examples
#' lon.lat.osm("Eiffeltower")
#'
#' @export
lon.lat.osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())

  if (curl::has_internet() == FALSE) {
    message("No internet connection")
    return(NULL)
  }
  if (tryCatch(curl::nslookup("nominatim.openstreetmap.org"), error = function(e) return(FALSE))==FALSE) {
    message("Couldn't resolve host name.")
    return(NULL)
  }
  if (httr::http_error(
    gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
         'https://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
  )==TRUE) {
    message("got HTTP error.")
    return(NULL)
  }
  tryCatch(
    d <- jsonlite::fromJSON(
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
           'https://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )

  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}
#---------------------------------------------------------------


#' Pairwise Chi-Square Tests
#'
#' This function performs pairwise Chi-Square tests for two factors.
#'
#' @param A A factor with two or moew levels. The first variable.
#' @param B A factor with two or more levels. The second variable.
#' @param p.adjust.method A string specifying the method for adjusting p-values. Default is "bonferroni".
#' @return A data frame with the results of the pairwise Chi-Square tests. Includes the groups, Chi-Square statistic, degrees of freedom, p-values, adjusted p-values, and significance stars.
#' @importFrom stats chisq.test p.adjust
#' @importFrom utils combn
#' @details This function creates all possible pairs of levels of factor B and performs a Chi-Square test for each pair of B on variable A. The p-values are adjusted according to the specified method.
#' #' This function is created for educational purposes only. For exact p-values, consider using \code{reporttools::pairwise.fisher.test()}.
#' @examples
#' set.seed(123)
#' A <- factor(sample(c("Male", "Female"), 100, replace = TRUE))
#' B <- factor(sample(c("Location1", "Location2", "Location3"), 100, replace = TRUE))
#' pairwise.chisq.test(A, B, "holm")
#' @export
pairwise.chisq.test <- function(A, B, p.adjust.method="bonferroni"){

  if (!is.factor(A) | !is.factor(B)) {
    stop("Both objects need to be factors.")
  }

  # make a data frame
  df <- data.frame(A=A, B=B)

  # create all combinations
  kombi <- combn(levels(B), 2)

  # set up a dummy data frame
  result <- data.frame(group1 = "A",
                       group2 = "B",
                       xsquare = "9",
                       df = "9999",
                       pvalue = "9",
                       padjust = "9",
                       adjust = p.adjust.method,
                       sig = "")

  # go for each pair
  for (i in 1:(length(kombi)/2)) {
    help <- rbind(df[df$B==kombi[,i][1],],
                  df[df$B==kombi[,i][2],])
    pups <- chisq.test(help$A, help$B)
    sig = ""
    if(p.adjust(as.numeric(pups$p.value), method = p.adjust.method, n=(length(kombi)/2)) < 0.055) {sig="."}
    if(p.adjust(as.numeric(pups$p.value), method = p.adjust.method, n=(length(kombi)/2)) < 0.05)  {sig="*"}
    if(p.adjust(as.numeric(pups$p.value), method = p.adjust.method, n=(length(kombi)/2)) < 0.01)  {sig="**"}
    if(p.adjust(as.numeric(pups$p.value), method = p.adjust.method, n=(length(kombi)/2)) < 0.001) {sig="***"}
    loop <- data.frame(group1 = kombi[,i][1],
                       group2 = kombi[,i][2],
                       xsquare = as.numeric(pups$statistic),
                       df = as.numeric(pups$parameter),
                       pvalue = as.numeric(pups$p.value),
                       padjust = p.adjust(as.numeric(pups$p.value), method = p.adjust.method, n=(length(kombi)/2)),
                       adjust = p.adjust.method,
                       sig = sig
    )
    result <- rbind(result, loop)
  }
  # delete dummy entry
  result <- result[-1,]
  # fix row numbers
  row.names(result) <- NULL
  # return results
  return(result)
}
#----------------------------------------------------------------------#




#' Compare Linear Models
#'
#' This function fits and compares several models (linear, quadratic, cubic, exponential, logarithmic, sigmoidal, power, logistic) to a given set of dependent and independent variables. It returns either a summary of the models with their R-squared values or predicted values based on the models.
#'
#' @param dep A numeric vector representing the dependent variable.
#' @param ind A numeric vector representing the independent variable.
#' @param predict Logical. If TRUE, the function returns predicted values for each model. Defaults to FALSE.
#' @param steps Numeric. The step size for generating x-values for predictions. Only used if predict is TRUE. Defaults to 0.01.
#'
#' @return A data frame. If predict is FALSE, returns a data frame with the R-squared values for each model. If predict is TRUE, returns a data frame with the original data and predicted values for each model.
#' @importFrom stats lm nls
#' @examples
#' x <- c(6, 9, 12, 14, 30, 35, 40, 47, 51, 55, 60)
#' y <- c(14, 28, 50, 70, 89, 94, 90, 75, 59, 44, 27)
#' compare.lm(y, x)
#' compare.lm(y, x, predict=TRUE)
#'
#' @export
compare.lm <- function(dep, ind, predict=FALSE, steps=0.01){

  # lineares Modell
  lin <- lm(dep ~ ind)

  # quadratisches Modell
  q <- lm(dep ~ ind + I(ind^2))

  # kubisches Modell
  c <- lm(dep ~ ind + I(ind^2) + I(ind^3))

  # exponentielles Modell
  e <- lm(log(dep) ~ ind)

  # logarithmisches Modell
  l <- lm(dep ~ log(ind))

  # sigmoidales Modell
  s <- lm(log(dep) ~ I(1/ind))

  # potenz Modell
  p <- lm(log(dep) ~ log(ind))

  ## logistische Funktion
  logit <- nls(dep ~ SSlogis(ind, Asym, xmid, scal), start = list(Asym = max(dep), xmid = mean(ind), scal = 1))

  result <- data.frame(Modell = c("linear", "quadratisch", "kubisch", "exponentiell",
                                  "logarithmisch", "sigmoidal", "potenz"), #"logistisch"),
                       R.square = c(summary(lin)$r.squared,
                                    summary(q)$r.squared,
                                    summary(c)$r.squared,
                                    summary(e)$r.squared,
                                    summary(l)$r.squared,
                                    summary(s)$r.squared,
                                    summary(p)$r.squared))#,
                                    #summary(logit)$r.squared))
  if(predict==TRUE){
    # x-Werte
    pred.x <- seq(min(ind), max(ind), steps)

    # linear
    pred.lin <- predict(lin, list(ind=pred.x))

    # quadratisch
    pred.q <- predict(q, list(ind=pred.x))

    # kubisch
    pred.c <- predict(c, list(ind=pred.x))

    # exponentiell
    pred.e <- exp(predict(e, list(ind=pred.x)))

    # logarithmisch
    pred.l <- predict(l, list(ind=pred.x))

    # sigmoidal
    pred.s <- predict(s, list(ind=pred.x))
    pred.s[-1] <- exp(pred.s[-1])

    # potenz
    pred.p <- exp(predict(p, list(ind=pred.x)))

    # logistisch
    pred.logit <- predict(logit, list(ind=pred.x))

    # ergebnisse zurückgeben
    return(data.frame(x = ind, y=dep,
                      pred.x = pred.x,
                      line = pred.lin,
                      quad = pred.q,
                      cube = pred.c,
                      expo = pred.e,
                      loga = pred.l,
                      sigm = pred.s,
                      power = pred.p,
                      logistic = pred.logit))
  } else {
    return(result[order(result$R.square, decreasing = TRUE),])
  }
}
