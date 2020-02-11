#generate sample data
candidates <- c("Sanders","Biden","Warren","Buttigieg","Steyer","Bloomberg","Yang","Klobuchar","Gabbard","Patrick")
#corresponding probabilities 
#https://projects.fivethirtyeight.com/polls/president-primary-d/nevada/
p <- c(.239,.212,.131,.083,.065,.057,0.037,0.033,0.013,0.01) 

n_early <- 10000 #number of early voters in a given precinct. no idea what this should be

#generate the *number* of candidates each voter ranks: I'm assuming this can be anywhere between 1 and the total number of candidates
#in practice I think it only be three to five candidates: https://lasvegassun.com/news/2020/feb/02/caucus-101-how-to-participate-in-nevadas-democrati/
n_ranked <- sample(1:length(candidates),n_early,replace=TRUE)

#simulate ranked voting. assuming subsequent choices are independent of prior choices. 
#obviously this is a poor assumption and it won't represent actual data but the point is just to generate some sample data to test the code
early_votes_l <- lapply(n_ranked, function(x){sample(candidates,x,replace=FALSE,prob=p)}) 


#early_votes_l is a ragged list (each element potentially of different length). turn it into a data.table
early_votes_l2 <- list()
for(i in 1:length(early_votes_l)){
  early_votes_l2[[i]] <- data.table(candidate=early_votes_l[[i]])
  early_votes_l2[[i]][,early_voter_id:=i]
}
early_votes <- rbindlist(early_votes_l2)
early_votes[,rank:=1:.N,by="early_voter_id"]

#generate first alignment table. easy.
table(sapply(early_votes_l,`[[`,1))

library(data.table)




#generate every possibly viability scenario_id after first round:
viability_threshold <- .15
max_viable_candidates <- floor(1/viability_threshold)
l <- list()
##all possible combinations of the candidates from size=1 to the maximum possible number of viable thresholds
#l is a ragged list (each element possibly of different length)
for(i in 1:max_viable_candidates){
  l <- c(l, combn(candidates,m=i,simplify=FALSE))
}

#turn ragged list into a data.table
dtlist <- list()

for(i in 1:length(l)){
  dtlist[[i]] <- data.table(candidate=l[[i]])
  dtlist[[i]][,scenario_id:=i]
}

scenarios <- rbindlist(dtlist)



create_unused_name <- function(x,reserved_cols){
  for(i in 1:length(x)){
    while(x[i] %in% reserved_cols){
      x[i] <- paste0("i.",x[i])
    }  
  }
  x
}


#a function to grid expand an arbitrary number of data.tables
#largely based on https://github.com/lockedata/optiRum/blob/master/R/CJ.dt.R
#groups is a character vector corresponding to column names of grouping vars  
#in all of the data.tables
CJ.dt <- function(...,groups=NULL) {
  l = list(...)
  EVAL <- function(...)eval(parse(text=paste0(...)))
  if(any(sapply(l,nrow)==0)){stop("one or more data.tables have no rows")}
  
  
  #while kvar is in names of any of the data.tables, keep prepending i. to it until it's not
  kvar <- create_unused_name("k",unlist(lapply(l,names)))
  
  invars <- create_unused_name(paste0("in",1:length(l)),
                               unlist(lapply(l,names)))
  
  for(i in 1:length(l)){
    l[[i]][,(kvar):=1]
    l[[i]][,(invars[i]):=TRUE]
    setkeyv(l[[i]],c(kvar,groups))
  }
  
  mymerge = function(x,y) x[y, allow.cartesian = TRUE]
  out <- Reduce(mymerge,l)
  out[,(kvar):=NULL]
  
  for(i in 1:length(l)){
    l[[i]][,(kvar):=NULL]
    l[[i]][,(invars[i]):=NULL]
  }
  
  out <- EVAL("out[",paste0(paste0("!is.na(",invars,")"),collapse="&"),"]")
  out[,(invars):=NULL]
  out[]
}


early_votes_expanded <- CJ.dt(early_votes,data.table(scenario_id=unique(scenarios[["scenario_id"]])))

setkeyv(early_votes_expanded,c("scenario_id","candidate"))
setkeyv(scenarios,c("scenario_id","candidate"))

early_votes_expanded2 <- early_votes_expanded[scenarios]

round2votes <- early_votes_expanded2[, list(candidate=candidate[min(rank)]),by=c("early_voter_id","scenario_id")]
round2votes[order(scenario_id, early_voter_id)]


round2votes[,stopifnot(sum(duplicated(early_voter_id))==0),by=scenario_id]


example_scenario <-c("Sanders", "Biden", "Warren", "Buttigieg")
example_scenario_id <- scenarios[,list(V1=all(example_scenario %in% candidate)&.N==length(example_scenario)),by="scenario_id"][(V1)][["scenario_id"]]


scenarios[scenario_id==example_scenario_id ]
round2votes[scenario_id==example_scenario_id , table(candidate)]
