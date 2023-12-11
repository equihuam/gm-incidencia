library(tidyverse)

patients <- tibble(
  PatientID = c('PID01','PID02'),
  PatientName = c('John Doe','Jane Doe'),
  PatientGroup = c('Group A','Group B')
)

encounters <- tibble(
  EncounterID = c('Enc01','Enc02','Enc03','Enc04','Enc05'),
  PatientID = c('PID01','PID01','PID02','PID02','PID02'),
  EncounterType = c('Outpatient','Outpatient','Inpatient','Outpatient','SNF')
)

encounterLines <- tibble(
  EncounterID = c(rep('Enc01',5),rep('Enc04',2)),
  RevCodes = c('001','100','200','300','400','001','100'),
  ClaimLine = c(seq(1:5),seq(1:2))
)

library(jsonlite)

json <- reduce(list(patients %>% mutate_if(is.factor, as.character),
                    encounters %>% mutate_if(is.factor, as.character),
                    encounterLines %>% mutate_if(is.factor, as.character) %>%
                    group_by(EncounterID) %>%
                    nest() %>%
                    rename(ClaimLines = data) %>%
                    mutate(ClaimLines = map(ClaimLines, transpose))),
                    left_join) %>%
                    nest(Encounters = c(EncounterID, EncounterType, ClaimLines)) %>%
                    transpose() %>%
                    toJSON(pretty = TRUE)


write(json, "./posts/gs-sqlite-contenido/data/test-json-nested.json")


subM_temas <- conceptos %>% select(tema, concepto, id)
subM_concept_proy <- conceptos %>% select(tema, concepto, id_p, id)

json <- reduce(list(subM_temas, 
                    subM_concept_proy %>% group_by(tema) %>% nest() %>%
                        rename(con = data) %>%
                        mutate(con = map(con, transpose))),
               left_join) %>%
  nest(conc = c(tema, tema, con)) %>%
  transpose() %>%
  toJSON(pretty = TRUE)


reduce(list(subM_concept_proy, subM_temas), left_join)

json <- nest(left_join(nest(subM_concept_proy, .by = tema), 
          nest(subM_temas, .by = tema), by = "tema"), .by = "tema")
json <- nest(subM_temas, .by = tema) %>% toJSON(pretty = TRUE)

