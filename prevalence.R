#rm(list=ls())

#library(RODBC)
#db <- odbcConnect(dsn='us-prd')

dbParam='Pharmetric_OMOP'
#date_low<-'2015-01-01'
#date_high<-'2015-06-30'

#inclusion_icd9<-paste('\'729.1\'',sep='')
#inclusion_ndc<-paste('\'31722022201\',\'23490651402\',\'18837005699\'',sep='')
#inclusion_proc<-paste('\'80366\',\'80171\',\'80355\'',sep='')


calculatePrevalence <- function(
  date_low, 
  date_high, 
  inclusion_icd9, 
  inclusion_ndc, 
  inclusion_proc, 
  database = dbParam)
{
  
  
  Pop_Prop<-read.csv('Census Population Proportion.csv',header=TRUE)
  colnames(Pop_Prop)<-c('name','sex1','age_group','pop2015','percent','state')
  Pop_Prop$sex<-ifelse(Pop_Prop$sex1=='1','M','F')
  
  if (database=='Pharmetric_OMOP')  {
    # Get the base population
    
    start_date <- as.Date(date_low) 
    end_date<- as.Date(date_high) 
    
    qry_base<-paste('select a.person_id,b.gender_source_value as sex,c.state,
                    ceiling((year(\'',start_date,'\') +year(\'',end_date,'\'))/2)-cast(b.year_of_birth as int) as age
                    from
                    (select distinct person_id 
                    from ims_pharmetrics_claims_omop_20153.payer_plan_period_history 
                    where payer_plan_period_start_date<=\'',start_date,'\'
                    and payer_plan_period_end_date>=\'',end_date,'\'
                    and medical_benefit=\'Y\' and drug_benefit=\'Y\') as a
                    inner join 
                    ims_pharmetrics_claims_omop_20153.person_history as b
                    on a.person_id=b.person_id
                    inner join
                    ims_pharmetrics_claims_omop_20153.location_history as c
                    on b.location_id=c.location_id',sep='')
    print(qry_base)
    
    dat_base <- sqlQuery(db,qry_base)
    
    
    # Check for ICD-9 included claims. At least one has to be present.
    qry_icd<-paste('select distinct a.person_id, a.condition_start_date as index_dt_icd
                   from ims_pharmetrics_claims_omop_20153.condition_occurrence_history as a
                   inner join
                   (select target_concept_id from ims_pharmetrics_claims_omop_20153.source_to_concept_map_history
                   where mapping_type=\'Condition\' and source_vocabulary_id=\'2\' 
                   and source_code in (',inclusion_icd9,') ) as b 
                   on a.condition_concept_id=b.target_concept_id
                   where a.condition_start_date>=\'',start_date,'\'
                   and a.condition_start_date<=\'',end_date,'\'',sep='')
    print(qry_icd)
    
    dat_icd <- sqlQuery(db,qry_icd)
    
    #Select the first ICD claims as index_dt
    ordered_data <- dat_icd[order(dat_icd$person_id, dat_icd$index_dt_icd),]
    dat_icd_f <- ordered_data[!duplicated(ordered_data$person_id),]
    dat_icd_f$icd<-'1'
    
    # Check for ndc included claims. At least one has to be present.
    qry_ndc<-paste('select distinct a.person_id, drug_exposure_start_date as index_dt_ndc
                   from ims_pharmetrics_claims_omop_20153.drug_exposure_history as a
                   inner join
                   (select target_concept_id from ims_pharmetrics_claims_omop_20153.source_to_concept_map_history
                   where mapping_type=\'Drug\' and source_vocabulary_id=\'9\' 
                   and source_code in (',inclusion_ndc,') ) as b 
                   on a.drug_concept_id=b.target_concept_id
                   where a.drug_exposure_start_date>=\'',start_date,'\'
                   and a.drug_exposure_start_date<=\'',end_date,'\'',sep='')
    print(qry_ndc)
    
    dat_ndc <- sqlQuery(db,qry_ndc)
    
    #Select the first ndc claims as index_dt
    ordered_data <- dat_ndc[order(dat_ndc$person_id, dat_ndc$index_dt_ndc),]
    dat_ndc_f <- ordered_data[!duplicated(ordered_data$person_id),]
    dat_ndc_f$ndc<-'1'
    
    # Check for procedure included claims. At least one has to be present.
    qry_proc<-paste('select distinct a.person_id, a.procedure_date as index_dt_proc
                    from ims_pharmetrics_claims_omop_20153.procedure_occurrence_history as a
                    inner join
                    (select target_concept_id from ims_pharmetrics_claims_omop_20153.source_to_concept_map_history
                    where mapping_type=\'Procedure\' and (source_vocabulary_id in (\'3\', \'4\'))
                    and source_code in (',inclusion_proc,') ) as b 
                    on a.procedure_concept_id=b.target_concept_id
                    where a.procedure_date>=\'',start_date,'\'
                    and a.procedure_date<=\'',end_date,'\'',sep='')
    print(qry_proc)
    
    dat_proc <- sqlQuery(db,qry_proc)
    
    #Select the first procedure claims as index_dt
    ordered_data <- dat_proc[order(dat_proc$person_id, dat_proc$index_dt_proc),]
    dat_proc_f <- ordered_data[!duplicated(ordered_data$person_id),]
    dat_proc_f$proc<-'1'
    
    #Merge Diagnosis, Drug and Procedure claims together, and identify the incident claims
    dat_int1<-merge(dat_icd_f,dat_ndc_f,by=c('person_id'),all=TRUE)
    dat_int2<-merge(dat_int1,dat_proc_f,by=c('person_id'),all=TRUE)
    
    dat_int3 <- data.frame(lapply(dat_int2, as.character), stringsAsFactors=FALSE)
    dat_int3$index_dt_proc[is.na(dat_int3$index_dt_proc)] <- '2099-01-01'
    dat_int3$index_dt_ndc[is.na(dat_int3$index_dt_ndc)] <- '2099-01-01'
    dat_int3$index_dt_icd[is.na(dat_int3$index_dt_icd)] <- '2099-01-01'
    dat_int3[is.na(dat_int3)] <- '0'
    
    #select incident from icd,ndc,procedure claims for each patient
    dat_int3$index_dt<-apply(dat_int3[,c(2,4,6)],1,min) 
    dat_int3$incident<-ifelse(dat_int3$index_dt==dat_int3$index_dt_icd,'icd',
                              ifelse(dat_int3$index_dt==dat_int3$index_dt_ndc,'ndc','proc'))
    
    #Reweight base population
    dat_base$age_group<-ceiling((dat_base$age+1)/5)
    dat_base_f1<-merge(dat_base,Pop_Prop,by=c('age_group','sex','state'),all.x=TRUE)
    dat_base_f2<-dat_base_f1[complete.cases(dat_base_f1[,9]),]
    dat_base_f2$rawper<-1/nrow(dat_base_f2)
    total_pop<-nrow(dat_base_f2)
    
    groupColumns = c("state","sex","age_group","percent")
    dataColumns = c("rawper")
    dat_base_f3 = ddply(dat_base_f2, groupColumns, function(x) colSums(x[dataColumns]))
    
    dat_base_f3$wgt<-dat_base_f3$percent/dat_base_f3$rawper
    
    dat_base_f4<-merge(dat_base_f2,dat_base_f3,by=c('age_group','sex','state'),all.x=TRUE)
    total_pop_reweight<-sum(dat_base_f4$wgt)
    
    #Reweight prevalence
    dat_disease_f<-merge(dat_base_f4,dat_int3,
                         by=c('person_id'),all.x=TRUE)
    dat_disease_f2<-dat_disease_f[complete.cases(dat_disease_f[,20]),]
    
    total_disease<-nrow(dat_disease_f2)
    total_disease_reweight<-sum(dat_disease_f2$wgt)
    
    prev<-total_disease/total_pop
    prev_re<-total_disease_reweight/total_pop_reweight
    
    dat<-dat_disease_f2[,-c(6,7,8,9,10,11,12,13,14,16,18)]
    
  }
  return(list(prev,prev_re,dat))
  
  fout <- paste('Period_Prevalence','.csv',sep='')
  write.csv(dat,file=fout,row.names=FALSE,col.names=TRUE)
  
}


