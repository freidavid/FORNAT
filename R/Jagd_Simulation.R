#' Function to simulate hunting scenarios and its effects on a red deer population, adapted from Wilson 2019 (https://bora.uib.no/bora-xmlui/bitstream/handle/1956/20034/msc_thesis_helene_wilson.pdf?sequence=1&isAllowed=y)
#'
#' @param J The number of years to simulate. Defaults to 10.
#' @param s The number of simulations to run. Defaults to 1.
#' @param l Hunting limit. At least l individuals of a age or sex group need to be in the population for hunting to take place. If there are less, there is no hunting. Defaults to 0. 
#' @param hch Number of harvested hind calves in each year. Defaults to 0.
#' @param hcs Number of harvested stag calves in each year. Defaults to 0.
#' @param hyh Number of harvested young hinds in each year. Defaults to 0.
#' @param hys Number of harvested young stags in each year. Defaults to 0.
#' @param hh Number of harvested hinds in each year. Defaults to 0.
#' @param hs Number of harvested stags in each year. Defaults to 0.
#' @param imax Carrying capacity of the popoulation. Defaults to 150.
#' @param c Maximum impact from carrying capacity. Defauults to 0.3.
#' @param a Slope of carrying capacity curve. Defaults to 1. 
#' @param m Number of individuals in the starting population.
#' @param age_distribution Age distribution in the starting population, eg. c(0.2,0.4,0.4). Distributes all individuals over the given number of groups in the specificied proportions. Defaults to 1.
#' @param sex_ratio Sex ratio in of new birhts, in the format c(female,male). Defaults to c(0.5,0.5).
#' @param average_offspring Average number of offspring per female. The number is drawn from a truncated poisson distribution for each birth event. Min and max can be specified separately. Defaults to 1. 
#' @param min_offspring Minimum number of offspring per female. Defaults to 1. 
#' @param max_offspring Maximum number of offspring per female. Defaults to 1. 
#' @param max_age Maximum age in the starting population.
#' @param min_repr Minimum age of reproduction.
#' @param max_repr Maximum age for reproduction.
#' @param pr_1 Probability of reproduction in the first year of reproduction. Defaults to 0.3.
#' @param pr_1_plus Probability of reproduction in all following years. Defaults to 0.9.
#' @param pr_d_0 Probability of death in the first year. Defaults to 0.3
#' @param alpha Transparence level of the lines in the population size plot. Defaults to 1.
#' @param plot_start Plot age and sex distribution of starting population. Defaults to F.
#' @param main Plot title for the population size over time plot.
#' @param ylim Ylim for the population size plot. If ylim is not specified, ylim values are inferred from the data.

 

#' 
#' @return list() with results and plots
#'
#' @author David Frei, \email{david.frei@fornat.com}
#'
#'
#' @import readxl
#' @import ggplot2
#' @import RColorBrewer
#' @import FSA
#' @import xlsx
#' @export





#########################################################################################################################################################
jagd_simulation <- function(J=10, s=1, l=0, hch=0, hcs=0, hyh=0, hys=0, hh=0, hs=0, imax=150, c=0.3, a=1, m=100, age_distribution=c(1), pr_1=0.3, pr_1_plus=0.9, sex_ratio=c(0.5,0.5), average_offspring=1,min_offspring=1,max_offspring=1, min_repr=1, max_repr=12, max_age=16, pr_d_0 = 0.15 ,alpha=1, plot_start=F, ylim=NULL, main="Populationsgrösse"){
  #########################################################################################################################################################
  
  
  # Generierung einer Startpopulation
  generate_population <- function(m, age_distribution, sex_ratio, max_age) {
    population <- list()
    age_groups <- length(age_distribution)
    interval<-round(max_age/age_groups,digits=0)
    
    
    for (i in 1:m) {
      # Zufällige Altersgruppe basierend auf der gegebenen Altersverteilung
      age_group <- sample(1:age_groups, 1, prob = age_distribution)
      # Zufälliges Alter innerhalb der ausgewählten Altersgruppe und unter Berücksichtigung des maximalen Alters
      
      max_age_in_group <- interval*age_group
      min_age <- interval*(age_group-1)
      age <- sample(min_age:max_age_in_group, 1)
      
      # Zufälliges Geschlecht basierend auf dem gegebenen Geschlechterverhältnis
      sex <- sample(c("female", "male"), 1, prob = sex_ratio)
      
      individual <- list(age = age, sex = sex)
      population <- c(population, list(individual))
    }
    
    return(population)
  }
  starting_population <- generate_population(m, age_distribution, sex_ratio, max_age)
  
  #######################################################################################################################################################
  
  # Funktion zur Aktualisierung des Alters für eine Population
  update_age <- function(population) {
    # Iteriere über jedes Individuum in der Population
    for (i in seq_along(population)) {
      # Zugriff auf das Alter jedes Individuums und erhöhe es um 1
      population[[i]]$age <- population[[i]]$age + 1
    }
    return(population)  # Rückgabe der aktualisierten Population
  }
  
  #########################################################################################################################################################
  # Funktion zur Erzeugung trunkierter Poisson-verteilte Zufallszahlen
  rtruncpois <- function(lambda, lower, upper, n = 1) {
    result <- NULL
    while (is.null(result)) {
      sample <- rpois(n=1, lambda)
      if (sample >= lower & sample <= upper) {
        result <- sample
      }
    }
    return(result)
  }
  
  
  # Funktion zur Fortpflanzung der Individuen
  reproduction <- function(population, sex_ratio, average_offspring, min_offspring, max_offspring, pr_1, pr_1_plus, min_repr, max_repr) {
    for (i in seq_along(population)) {
      age <- population[[i]]$age  # Zugriff auf das Attribut 'age' des Individuums
      sex <- population[[i]]$sex
      reproducing <- ifelse("reproducing" %in% names(population[[i]]), population[[i]]$reproducing, FALSE)  # Zugriff auf das Attribut 'reproducing' des Individuums
      
      # Bedingungen für Fortpflanzung basierend auf dem Alter und anderen Faktoren
      if (age >= min_repr) {
        population[[i]]$reproducing <- TRUE  # Setze reproducing auf TRUE, wenn age größer oder gleich 1 ist
      } else {
        if (!"reproducing" %in% names(population[[i]])) {
          population[[i]]$reproducing <- FALSE  # Setze reproducing auf FALSE, wenn age kleiner als 1 ist und reproducing noch nicht existiert
        }
      }
      
      if (age == min_repr & reproducing == TRUE & sex == "female") {
        if (any(sapply(population, function(x) x$age >= min_repr))) {
          pr <- pr_1
          if (runif(1) < pr) {
            #sex <- sample(c("female", "male"), 1, prob = sex_ratio, replace = TRUE)
            #new_individual <- list(age = 0, sex = sex, reproducing = FALSE)
            #population <- c(population, list(new_individual))
            offspring_count <- rtruncpois(lambda = average_offspring, lower = min_offspring, upper = max_offspring, n = 1 )  # Zufällige Anzahl von Nachkommen gemäß einer Poisson-Verteilung mit durchschnittlicher Anzahl von Nachkommen
            for (j in 1:offspring_count) {
              sex <- sample(c("female", "male"), 1, prob = sex_ratio, replace = TRUE)
              new_individual <- list(age = 0, sex = sex, reproducing = FALSE)
              population <- c(population, list(new_individual))
            }
          }
        }
      } else if (min_repr < age & age < max_repr & reproducing == TRUE & sex == "female") {
        if (any(sapply(population, function(x) x$age >= min_repr))) {
          pr <- pr_1_plus
          if (runif(1) < pr) {
            #sex <- sample(c("female", "male"), 1, prob = sex_ratio, replace = TRUE)
            #new_individual <- list(age = 0, sex = sex, reproducing = FALSE)
            #population <- c(population, list(new_individual))
            offspring_count <- rtruncpois(lambda = average_offspring, lower = min_offspring, upper = max_offspring, n = 1)  # Zufällige Anzahl von Nachkommen gemäß einer Poisson-Verteilung mit durchschnittlicher Anzahl von Nachkommen
            for (j in 1:offspring_count) {
              sex <- sample(c("female", "male"), 1, prob = sex_ratio, replace = TRUE)
              new_individual <- list(age = 0, sex = sex, reproducing = FALSE)
              population <- c(population, list(new_individual))
            }
          }
        }
      }
    }
    return(population)  # Rückgabe der aktualisierten Population
  }
  
  #########################################################################################################################################################
  # Funktion zur Berechnung der Todesrate basierend auf Alter und Dichte (aktuelle Popgrösse / max. Popgrösse)
  
  calculate_pi_d_with_capacity <- function(population, imax, c, a, max_age, pr_d_0) {
    for (i in seq_along(population)) {
      age <- population[[i]]$age
      inow<-length(population)
      if (age == 0) {
        pi_d <- pr_d_0
      } else if (age < max_age) {
        pi_d <- 0.03 + (0.05/14 * (age - 1))
      } else {
        pi_d <- 0.08 * exp(2.47 * (age - max_age))
      }
      
      # Calculate the increment based on carrying capacity
      increment <- c/2 * (1 + tanh(a * (inow - imax)))
      # Update pi_d with the increment
      updated_pi_d <- pi_d + increment
      # Store the updated pi_d in the individual
      population[[i]] <- c(population[[i]], pi_d = updated_pi_d)
    }
    return(population)
  }
  
  #########################################################################################################################################################
  
  # Funktion zur Berechnung der natürlichen Tode
  death_function <- function(population, max_age) {
    population <- Filter(function(individual) {
      ia <- individual$age
      pi_d <- individual$pi_d
      
      if (ia == 0 || (ia > 0 & ia < max_age) || ia >= max_age) {
        if (runif(1) >= pi_d) {
          # Individual survives
          return(TRUE)
        } else {
          # Individual dies and is removed from population
          #cat("Individual died: Age:", ia, ", pi_d:", pi_d, "\n")
          return(FALSE)
        }
      }
    }, population)
    
    return(population)
  }
  
  #########################################################################################################################################################
  
  # Funktion zur Berechnung der Jagd
  hunting_function <- function(population, l, hch, hcs, hyh, hys, hh, hs) {
    calves_hinds <- 0
    calves_stags <- 0
    young_hinds <- 0
    young_stags <- 0
    hinds <- 0
    stags <- 0
    
    for (individual in population) {
      ia <- individual$age
      is_calf_hind <- ia == 0 && individual$sex == "female"
      is_calf_stag <- ia == 0 && individual$sex == "male"
      is_young_hind <- ia == 1 && individual$sex == "female"
      is_young_stag <- ia == 1 && individual$sex == "male"
      is_hind <- ia > 1 && individual$sex == "female"
      is_stag <- ia > 1 && individual$sex == "male"
      
      calves_hinds <- calves_hinds + is_calf_hind
      calves_stags <- calves_stags + is_calf_stag
      young_hinds <- young_hinds + is_young_hind
      young_stags <- young_stags + is_young_stag
      hinds <- hinds + is_hind
      stags <- stags + is_stag
    }
    
    if (calves_hinds > l) {
      population <- population[-which(sapply(population, function(x) x$age == 0 & x$sex == "female"))[1:min(hch, sum(sapply(population, function(x) x$age == 0 & x$sex == "female")))]]
    }
    if (calves_stags > l) {
      population <- population[-which(sapply(population, function(x) x$age == 0 & x$sex == "male"))[1:min(hcs, sum(sapply(population, function(x) x$age == 0 & x$sex == "male")))]]
    }
    
    if (young_hinds > l) {
      population <- population[-which(sapply(population, function(x) x$age == 1 & x$sex == "female"))[1:min(hyh, sum(sapply(population, function(x) x$age == 1 & x$sex == "female")))]]
    }
    if (young_stags > l) {
      population <- population[-which(sapply(population, function(x) x$age == 1 & x$sex == "male"))[1:min(hys, sum(sapply(population, function(x) x$age == 1 & x$sex == "male")))]]
    }
    if (hinds > l) {
      population <- population[-which(sapply(population, function(x) x$age > 1 & x$sex == "female"))[1:min(hh, sum(sapply(population, function(x) x$age > 1 & x$sex == "female")))]]
    }
    if (stags > l) {
      population <- population[-which(sapply(population, function(x) x$age > 1 & x$sex == "male"))[1:min(hs, sum(sapply(population, function(x) x$age > 1 & x$sex == "male")))]]
    }
    
    return(population)
  }
  
  #########################################################################################################################################################
  
  # Hauptalgorithmus für eine Generation, bzw. 1 Jahr
  main_algorithm <- function(population, l, hch, hcs ,hyh, hys, hh, hs, imax, c, a, sex_ratio, average_offspring, min_offspring, max_offspring, pr_1, pr_1_plus, max_age, min_repr, max_repr, pr_d_0 ) {
    sample_space <- list()
    pop_size<-list()
    
    
    
    # Schrittweise Ausführung der Algorithmen
    population <- update_age(population)
    population <- reproduction(population, sex_ratio, average_offspring, min_offspring, max_offspring, pr_1, pr_1_plus, min_repr, max_repr)
    population<-calculate_pi_d_with_capacity(population, imax, c, a, max_age, pr_d_0)
    population <- death_function(population, max_age)
    population <- hunting_function(population, l, hch, hcs, hyh, hys, hh, hs)
    
    
    #sample_space[[i]] <- population
    pop_size <- length(population)
    
    return(list(sample_space=population,pop_size=pop_size))
  }
  
  
  ########################################################################################################################################################
  
  # Aufruf des Hauptalgorithmus
  
  resultate<-list()
  population<-starting_population
  
  for(n in 1:s){
    
    out<-list()
    populationsgroesse<-vector()
    
    out[[1]]<-main_algorithm(starting_population, l, hch, hcs, hyh, hys, hh, hs, imax, c, a, sex_ratio, average_offspring, min_offspring, max_offspring, pr_1, pr_1_plus, max_age, min_repr, max_repr, pr_d_0 )
    populationsgroesse[1]<-unlist(out[[1]]$pop_size)
    
    for(j in 2:J){
      population<-out[[j-1]]$sample_space
      out[[j]]<-main_algorithm(population, l, hch, hcs, hyh, hys, hh, hs, imax, c, a, sex_ratio, average_offspring, min_offspring, max_offspring, pr_1, pr_1_plus, max_age, min_repr, max_repr, pr_d_0 )
    }
    resultate[[n]]<-out
  }
  
  ########################################################################################################################################################
  
  
  #Plotting    
  
  if(plot_start==T){
    #Startpopulation
    ages<-vector()
    for(i in 1:length(starting_population)){
      ages[i]<-starting_population[[i]]$age}
    hist(ages,main="Altersverteilung Startpopoulation")
    
    sexes<-vector()
    for(i in 1:length(starting_population)){
      sexes[i]<-starting_population[[i]]$sex}
    sexes<-as.numeric(factor(sexes))
    
    hist(sexes,breaks=2,axes=F, main="Geschlechterverteilung Startpopulation")
    axis(2)
    axis(1,at=c(1.25,1.75),labels=c("w","m"))
  }
  
  
  #Populationsgrösse über die Zeit
  # Leerer Vektor zum Speichern der pop_size-Werte für jedes resultate[[i]]
  pop_size_lists <- list()
  
  # Iteriere über jedes Element in resultate und extrahiere den pop_size-Wert
  for (i in seq_along(resultate)) {
    pop_sizes <- sapply(resultate[[i]], function(x) x$pop_size)
    pop_size_lists[[i]] <- pop_sizes
  }
  
  
  alpha=alpha
  if(length(ylim)==0){ylim=c(min(unlist(pop_size_lists))*0.95,max(unlist(pop_size_lists))*1.05)}
  plot(1:J,unlist(pop_size_lists[[1]]),ylim=ylim,type="l",ylab="Populationsgrösse",xlab="Anzahl Jahre",col=rgb(0, 0, 0, alpha = alpha),main=main)
  
  
  if(s>1){
    for(k in 2:s){
      points(1:J,unlist(pop_size_lists[[k]]),col=rgb(0, 0, 0, alpha = alpha),type="l")
    }
  }
  
  #durchschnittliche Populationsgrösse über alle Jahre
  int<-matrix(unlist(pop_size_lists),ncol=n)
  mean_pop_size<-rowSums(int)/n
  
  #varianzen für jedes Jahr
  varianzen<-vector()
  for(i in 1:J){varianzen[i]<-var(int[i,])}
  
  #hinzufügen der infos zum plot
  mtext(paste("Durchschn. Populationsgrösse =",round(mean(unlist(pop_size_lists)),digits=1),",Durchschn. Varianz =",round( mean(varianzen),digits=1)),cex=0.7)
  points(1:J,mean_pop_size,col="red",type="l",lwd=2)
  
  
  ########################################################################################################################################################              
  ausgabe<-list(starting_population=starting_population,resultate=resultate,pop_groessen=pop_size_lists)
  return(ausgabe)
  
  
}
#########################################################################################################################################################
