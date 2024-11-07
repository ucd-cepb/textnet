# Internal function 
# verbnet_port

#' Links to VerbNet 3.3 and uses the classification structure to create a data.table
#' where each row is a verb, and the columns represent the types and classifications in which that
#' verb can be found. Also adds a designation for "helping verbs."
#'
#' @param zipdestfile A filepath on which to store the zipfiles downloaded from VerbNet
#' @param folder_dest A filepath for the folder in which to put the extracted VerbNet files
#' 
#' @import data.table
#' @importFrom utils download.file untar
#' @importFrom R.utils gunzip
#' @importFrom xml2 read_xml xml_attr xml_find_all
#' @importFrom base strsplit unique
#' @importFrom dplyr case_when
#' @return Returns the data.table of verbs and their classifications.
#' 
#' 

#when generating the data file verb_classifications in the TextNet package,
#zipdestfile was set to "data/verbnet3.3"
#and folder_dest was set to "data"

verbnet_port <- function(zipdestfile, folder_dest){
  # Input validation
  if(!is.character(zipdestfile) || length(zipdestfile) != 1) {
    stop("'zipdestfile' must be a single character string")
  }
  
  if(!is.character(folder_dest) || length(folder_dest) != 1) {
    stop("'folder_dest' must be a single character string") 
  }

  url <- "https://verbs.colorado.edu/verb-index/vn/verbnet-3.3.tar.gz"
  utils::download.file(url, paste0(zipdestfile,".tar.gz"), method="curl")
  R.utils::gunzip(paste0(zipdestfile,".tar.gz"))
  utils::untar(paste0(zipdestfile,".tar"),exdir=folder_dest)
  
  
  
  files <- list.files(path=paste0(folder_dest,"/verbnet3.3"), pattern = ".xml")
  classes <- unname(sapply(files, function(i) base::strsplit(i, split = "-")[[1]][1]))
  classes
  
  verbnet <- vector(mode='list', length=length(files))
  #subcategories: organizational structuring/
  verbnet <- lapply(files, function (m) xml2::read_xml(paste0("data/verbnet3.3/",m)))
  
  #xml_find_all(verbnet2, "/pathway//entry")
  members <- vector(mode='list',length=length(files))
  members <- lapply(verbnet, function(m) xml2::xml_attr(xml2::xml_find_all(m, xpath = "//VNCLASS/MEMBERS/MEMBER"),"name"))
  submembers <- vector(mode='list',length=length(files))
  submembers <- lapply(verbnet, function (m) xml2::xml_attr(xml2::xml_find_all(m, xpath = "//VNCLASS/SUBCLASSES/VNSUBCLASS/MEMBERS/MEMBER"),"name"))
  members
  submembers
  
  
  type_ids <- unname(sapply(files, function(i) base::strsplit(i, split = "-|\\.")[[1]][2]))
  type_names <- dplyr::case_when(
    type_ids == 9 ~ "verbs_of_putting",
    type_ids == 10 ~ "verbs_of_removing",
    type_ids == 11 ~ "verbs_of_sending_and_carrying",
    type_ids == 12 ~ "verbs_of_exerting_force-push-pull_verbs",
    type_ids == 13 ~ "verbs_of_change_of_possession",
    type_ids == 14 ~ "learn_verbs",
    type_ids == 15 ~ "hold_and_keep_verbs",
    type_ids == 16 ~ "verbs_of_concealment",
    type_ids == 17 ~ "verbs_of_throwing",
    type_ids == 18 ~ "verbs_of_contact_by_impact",
    type_ids == 19 ~ "poke_verbs",
    type_ids == 20 ~ "verbs_of_contact-touch_verbs",
    type_ids == 21 ~ "verbs_of_cutting",
    type_ids == 22 ~ "verbs_of_combining_and_attaching",
    type_ids == 23 ~ "verbs_of_separating_and_disassembling",
    type_ids == 24 ~ "verbs_of_coloring",
    type_ids == 25 ~ "image_creation_verbs",
    type_ids == 26 ~ "verbs_of_creation_and_transformation",
    type_ids == 27 ~ "engender_verbs",
    type_ids == 28 ~ "calve_verbs",
    type_ids == 29 ~ "verbs_with_predicative_complements",
    type_ids == 30 ~ "verbs_of_perception",
    type_ids == 31 ~ "psych-verbs-verbs_of_psychological_state",
    type_ids == 32 ~ "verbs_of_desire",
    type_ids == 33 ~ "judgment_verbs",
    type_ids == 34 ~ "verbs_of_assessment",
    type_ids == 35 ~ "verbs_of_searching",
    type_ids == 36 ~ "verbs_of_social_interaction",
    type_ids == 37 ~ "verbs_of_communication",
    type_ids == 38 ~ "verbs_of_sounds_made_by_animals",
    type_ids == 39 ~ "verbs_of_ingesting",
    type_ids == 40 ~ "verbs_involving_the_body",
    type_ids == 41 ~ "verbs_of_grooming_and_bodily_care",
    type_ids == 42 ~ "verbs_of_killing",
    type_ids == 43 ~ "verbs_of_emission",
    type_ids == 44 ~ "destroy_verbs",
    type_ids == 45 ~ "verbs_of_change_of_state",
    type_ids == 46 ~ "lodge_verbs",
    type_ids == 47 ~ "verbs_of_existence",
    type_ids == 48 ~ "verbs_of_appearance_disappearance_and_occurrence",
    type_ids == 49 ~ "verbs_of_body-internal_motion",
    type_ids == 50 ~ "verbs_of_assuming_a_position",
    type_ids == 51 ~ "verbs_of_motion",
    type_ids == 52 ~ "avoid_verbs",
    type_ids == 53 ~ "verbs_of_lingering_and_rushing",
    type_ids == 54 ~ "measure_verbs",
    type_ids == 55 ~ "aspectual_verbs",
    type_ids == 56 ~ "weekend_verbs",
    type_ids == 57 ~ "weather_verbs",
    type_ids == 58 ~ "verbs_of_urging_and_begging",
    type_ids == 59 ~ "force_verbs",
    type_ids == 60 ~ "attack_verbs",
    type_ids == 61 ~ "try_verbs",
    type_ids == 62 ~ "wish_verbs",
    type_ids == 63 ~ "enforce_verbs",
    type_ids == 64 ~ "allow_verbs",
    #type_ids == 65 ~ "admit_verbs", deprecated from verbnet 3.2
    type_ids == 66 ~ "consume_verbs",
    #type_ids == 67 ~ "forbid_verbs", deprecated from verbnet 3.2
    type_ids == 68 ~ "pay_verbs",
    type_ids == 69 ~ "refrain_verbs",
    type_ids == 70 ~ "rely_verbs",
    type_ids == 71 ~ "conspire_verbs",
    type_ids == 72 ~ "help_verbs",
    type_ids == 73 ~ "cooperate_verbs",
    type_ids == 74 ~ "succeed_verbs",
    type_ids == 75 ~ "neglect_verbs",
    type_ids == 76 ~ "limit_verbs",
    type_ids == 77 ~ "approve_verbs",
    type_ids == 78 ~ "indicate_verbs",
    type_ids == 79 ~ "dedicate_verbs",
    type_ids == 80 ~ "free_verbs",
    type_ids == 81 ~ "suspect_verbs",
    type_ids == 82 ~ "withdraw_verbs",
    type_ids == 83 ~ "cope_verbs",
    type_ids == 84 ~ "discover_verbs",
    type_ids == 85 ~ "cognize_verbs",
    type_ids == 86 ~ "verbs_of_correlating_and_relating",
    type_ids == 87 ~ "verbs_of_focusing_and_comprehending",
    type_ids == 88 ~ "verbs_of_caring_and_empathizing",
    #type_ids == 89 ~ "settle_verbs", deprecated from verbnet 3.2
    type_ids == 90 ~ "exceed_verbs",
    type_ids == 91 ~ "matter_verbs",
    type_ids == 92 ~ "confine_verbs",
    type_ids == 93 ~ "adopt_verbs",
    type_ids == 94 ~ "risk_verbs",
    type_ids == 95 ~ "acquiesce_verbs",
    type_ids == 96 ~ "addict_verbs",
    type_ids == 97 ~ "verbs_of_basing_and_deducing",
    type_ids == 98 ~ "confront_verbs",
    type_ids == 99 ~ "ensure_verbs",
    type_ids == 100 ~ "own_verbs",
    type_ids == 101 ~ "patent_verbs",
    type_ids == 102 ~ "promote_verbs",
    type_ids == 103 ~ "require_verbs",
    type_ids == 104 ~ "verbs_of_spending_time",
    type_ids == 105 ~ "use_verbs",
    type_ids == 106 ~ "void_verbs",
    type_ids == 107 ~ "involve_verbs",
    type_ids == 108 ~ "multiply_verbs",
    type_ids == 109 ~ "seem_verbs",
    type_ids == 110 ~ "representation_verbs",
    type_ids == 111 ~ "conduct_verbs",
    type_ids == 112 ~ "reciprocate_verbs",
    type_ids == 113 ~ "respond_verbs",
    type_ids == 114 ~ "act_verbs",
    .default = NA_character_
  )
  
  members_and_submembers <- sapply(seq_along(members), function (m) base::unique(c(members[[m]],submembers[[m]])))
  class_dt <- data.table::data.table("class" = classes, "type_id" = type_ids, "type_name" = type_names,
                         "verbs" = members_and_submembers)
  
  
  verbs <- base::unique(unlist(class_dt$verbs))
  verb_classes <- lapply(verbs, function (j) base::unique(unlist(lapply(1:nrow(class_dt), 
                                                                  function (m) if(j %in% class_dt$verbs[[m]]) class_dt$class[m]))))
  
  verb_types <- lapply(verbs, function (j) base::unique(unlist(lapply(1:nrow(class_dt), 
                                                                function (m) if(j %in% class_dt$verbs[[m]]) class_dt$type_name[m]))))
  verb_type_ids <- lapply(verbs, function (j) base::unique(unlist(lapply(1:nrow(class_dt), 
                                                                   function (m) if(j %in% class_dt$verbs[[m]]) class_dt$type_id[m]))))
  
  verb_pivot <- data.table::data.table("verb" = verbs, "classes" = verb_classes, "type_name" = verb_types, "type_id" = verb_type_ids)
  
  helping_verbs <- data.table::data.table("verb" = c("be",
                                         "have","do","shall",
                                         "will","wo",
                                         "should","would","may","might","must",
                                         "can","could","ought",
                                         "dare","need"),
                              "classes" = "helping_verbs",
                              "type_name" = "helping_verbs",
                              "type_id" = 0)
  
  helping_verbs$classes <- unname(sapply(helping_verbs$verb, function (i)
    ifelse(i %in% verb_pivot$verb, 
           list(c(verb_pivot[verb_pivot$verb==i,"classes"][[1]][[1]],"helping_verbs")), "helping_verbs")))
  helping_verbs$type_name <- unname(sapply(helping_verbs$verb, function (i)
    ifelse(i %in% verb_pivot$verb, 
           list(c(verb_pivot[verb_pivot$verb==i,"type_name"][[1]][[1]],"helping_verbs")), "helping_verbs")))
  helping_verbs$type_id <- unname(sapply(helping_verbs$verb, function (i)
    ifelse(i %in% verb_pivot$verb, 
           list(c(verb_pivot[verb_pivot$verb==i,"type_id"][[1]][[1]],"0")), "0")))
  
  verb_classifications <- rbind(verb_pivot[!(verb_pivot$verb %in% helping_verbs$verb),], helping_verbs)
  
  #when generating the data file verb_classifications, the following line of code was run
  #save(verb_classifications, file = "data/verb_classifications.rda")
  
  return(verb_classifications)
}
