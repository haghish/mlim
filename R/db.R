#' @title creates and updates a database
#' @description creates and updates missing data imputation accuracy to a database
#' @param file filename
#' @param db loaded 'db' object
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
db <- function(mlim.error, algo, tuning_time="default", name, df,
               p, stratify,
               db=NULL, file=NULL){

  if (class(mlim.error) != "list") stop("mlim must be a list")

  df <- data.frame(algo=algo,
                   tuning_time = tuning_time,
                   name=name,
                   nrow=nrow(df),
                   ncol=ncol(df),
                   ncol.numeric = length(mlim.error$nrmse),
                   ncol.factor = length(mlim.error$missclass),
                   ncol.ordered= length(mlim.error$missrank),
                   p = p,
                   stratify = stratify,
                   nmrse = mlim.error$mean["nrmse"],
                   missclass = mlim.error$mean["missclass"],
                   missrank = mlim.error$mean["missrank"]
                   )

  if (!is.null(db)) df <- rbind(db, df)
  if (!is.null(file)) saveRDS(df, file=file)
  return(df)
}




