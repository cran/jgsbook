####################################################
# In order to let roxygen2() create all man pages, #
# you must run the following command:              #
#       roxygen2::roxygenise()                     #
####################################################


#' Datatable of the epa Example
#'
#' @details Variables in the dataset:
#' \itemize{
#'   \item sex. a factor with levels \code{m} \code{w} \code{d}, giving the proband's sex
#'   \item age. a numeric vector
#'   \item cms. a numeric vector
#'   \item risk. a dichotome vector, 0 = not at risk, 1 = at risk
#'   \item expert. a dichotome vector of expert's decision, 0 = not at risk, 1 = at risk
#'   \item decu. a dichotome vector, 0 = no decubitus, 1 = decubitus
#' }
#'
#' @title Datatable of the epa Example
#' @docType data
#' @keywords datasets
#' @name epa
#' @usage data(epa)
#' @format A data frame with 620 observations in 6 variables
#' @source \url{https://www.produnis.de/R/}
NULL





#' Dataset of a work sampling study
#'
#' @details Variables in the dataset:
#' \itemize{
#'   \item day. a vector, giving the number of the observation day
#'   \item time. a factor giving the time of observation
#'   \item ward. a factor giving the ward under observation
#'   \item qual. a factor giving the qualification of the nurse
#'   \item category. a factor of qualification categories
#'   \item action. a factor giving the observed action
#' }
#' @title Dataset of a work sampling study
#' @docType data
#' @keywords datasets
#' @name mma
#' @usage data(mma)
#' @format  A data frame with 9768 observations in 6 variables.
#' @source \url{https://www.produnis.de/R/}
NULL


#' Dataset of the German Nachtwachen study
#'
#' @title Dataset of the German Nachtwachen study
#' @docType data
#' @keywords datasets
#' @name Nachtwachen
#' @usage data(Nachtwachen)
#' @format  A data frame with 276 observations in 37 variables.
#' @source \url{https://www.produnis.de/R/}
NULL





#' Dataset of the German Nachtwachen study, labelled version
#'
#' @title Dataset of the German Nachtwachen study with labelled variables
#' @docType data
#' @keywords datasets
#' @name nw
#' @aliases nw_labelled
#' @usage data(nw)
#' @format  A data frame with 276 observations in 37 variables.
#' @source \url{https://www.produnis.de/R/}
NULL





#' Datatable of an Ordinal Sample
#'
#' @title Datatable of an Ordinal Sample
#' @docType data
#' @keywords datasets
#' @name OrdinalSample
#' @aliases ordinalSample
#' @usage data(OrdinalSample)
#' @format  A data frame with 415 observations in 4 variables.
#' @source \url{https://www.produnis.de/R/}
#' @details Variables in the dataset:
#' \itemize{
#'   \item Konflikt. a numeric vector giving the potential of conflicts.
#'   \item Zufriedenh. a numeric vector giving the satisfaction of workers
#'   \item Geschlecht. a factor of proband's sex, 1 = male, 2=female
#'   \item Stimmung.  an ordinal factor of proband's mood
#' }
NULL




#' This is the dataset of the PF8 example.
#'
#' @title Dataset of the PF8 example.
#' @docType data
#' @keywords datasets
#' @name pf8
#' @usage data(pf8)
#' @format  A data frame with 731 observations in 16 variables.
#' @source \url{https://www.produnis.de/R/}
NULL






#' Matrix of Pflegeberufe by Isfort et al. 2018
#'
#' @title Matrix of Pflegeberufe by Isfort et al. 2018
#' @docType data
#' @keywords datasets
#' @name Pflegeberufe
#' @usage data(Pflegeberufe)
#' @format  A matrix with 9 cols (years) and 5 rows (nursing profession).
#' @author Isfort et al. 2018 (Pflegethermometer)
#' @source \url{https://www.produnis.de/R/}
NULL



