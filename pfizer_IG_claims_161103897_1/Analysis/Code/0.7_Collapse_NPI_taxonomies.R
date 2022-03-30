#########################################################
#  Script: COllapse_NPI_Taxonomies.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 12/6/19
#  Date Edited: 12/6/19
########################################################


library(tidyverse)
library(data.table)
library(magrittr)
library(openxlsx)
library(DescTools)

load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med Claims for Site Identification clean txs.RData")
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/NPI_registry_deduped.RData")

med$attending_npi %>% is.na() %>% table()
med$billing_pr_npi %>% is.na() %>% table()
med$facility_npi %>% is.na() %>% table()



# take billing npi first, then attending npi, then facility NPI

med <- med %>% mutate(NPI = case_when( (is.na(billing_pr_npi) &
                                          is.na(attending_npi)) ~ facility_npi,
                                       is.na(billing_pr_npi) ~ attending_npi,
                                       TRUE ~ billing_pr_npi))
med$NPI %>% is.na() %>% table()
# missing NPI Identifier
med_no_NPI <- med %>% filter(is.na(NPI)) # 1308293
med_no_NPI$attending_npi %>% is.na() %>% table()
med_no_NPI$billing_pr_npi %>% is.na() %>% table()
med_no_NPI$facility_npi %>% is.na() %>% table()

#med <- med %>% filter(!is.na(NPI)) # 778376
med$patient_id %>% n_distinct() # 139917 patients
med$claim_id %>% n_distinct() # 6223060 claims



attending_npi <-  med$attending_npi %>% unique()
billing_npi <- med$billing_pr_npi %>% unique()
facility_npi <- med$facility_npi %>% unique()




# select out attending and billing NPIs from NPI registry

NPIreg <- NPIs %>% filter(NPI %in% attending_npi |
                                   NPI %in% billing_npi |
                                   NPI %in% facility_npi) %>% 
  select(NPI, provider_taxonomy_code, provider_taxonomy_desc) %>% na.omit()

NPIreg  <- NPIreg %>% distinct()
gc()

NPIreg %>% group_by(NPI) %>% filter(n()>1)
#Dedup
# 2344375
med <- med %>% distinct()


### medical claims with Idnentifying NPI
med2 <- med %>% left_join(NPIreg)


med3 <- med2 %>% mutate(NPIcollapsed = 
                          case_when( provider_taxonomy_desc == "Internal Medicine Allergy and Immunology" ~ "Allergy and Immunology"))

med2$NPIcollapsed <- med2$provider_taxonomy_desc %>%
  fct_collapse("Hospital" = c)
                          
                          
med2 <- med2 %>% mutate(NPIcollapsed = 
                  case_when(provider_taxonomy_desc == "Hospitals General Acute Care Hospital" ~ "Hospital",
                            provider_taxonomy_desc == "Agencies Public Health or Welfare" ~ NA_character_,
                            provider_taxonomy_desc ==  "Agencies Voluntary or Charitable" ~ NA_character_,
                            provider_taxonomy_desc ==  "Allopathic &Osteopathic Physicians Dermatology Procedural Dermatology" ~ NA_character_, 
                            provider_taxonomy_desc ==  "Allopathic and Osteopathic Physicians Advanced Heart Failure and Transplant Cardiology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Allergy and Immunology" ~ "Allergy and Immunology",
                            provider_taxonomy_desc ==  "Allopathic and Osteopathic Physicians Allergy and Immunology Allergy" ~ "Allergy and Immunology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Allergy and Immunology Clinical and Laboratory Immunology" ~ "Allergy and Immunology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Anesthesiology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Anesthesiology Critical Care Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Anesthesiology Pain Medicine" ~ NA_character_, 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Anesthesiology Pediatric Anesthesiology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Colon and Rectal Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Dermatology" ~ NA_character_, 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Dermatology Dermapathology" ~  NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Dermatology Pediatric Dermatology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Dermatology MOHS-Micrographic Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Emergency Medicine Emergency Medical Services" ~ 'Hospital',
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Emergency Medicine Medical Toxicology" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Emergency Medicine Pediatric Emergency Medicine" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine" ~ "Family Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine Adolescent Medicine" ~ "Family Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine Adult Medicine" ~ "Family Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine Geriatric Medicine" ~ "Family Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine Hospice and Palliative Medicine" ~ "Family Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians General Practice" ~ "General Practice",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Hospitalist" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Addiction Medicine" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Adolescent Medicine" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Allergy and Immunology" ~ "Allergy and Immunology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Cardiovascular Disease" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Bariatric Medicine" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Cardiovascular Disease" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Clinical and Laboratory Immunology" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Clinical Cardiatric Electrophysiology" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Colon and Rectal Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Critical Care Medicine" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Endocrinology Diabetes and Metabolism" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Gastroenterology" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Hematology" ~ "Hematology and Oncology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Hematology and Oncology"  ~ "Hematology and Oncology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Hepatology" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Infectious Disease" ~ "Infectious Disease",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Interventional Cardiology" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Medical Oncology" ~ "Hematology and Oncology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Nephrology" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Pulmonary Disease" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Rheumatology" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Sleep Medicine" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Transplant Hepatology" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Neurological Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Neuromusculoskeletal Medicine and OMM" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Neuromusculoskeletal Medicine Sports Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Nuclear Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Nuclear Medicine Nuclear Imaging and Therapy" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology Gynecologic Oncology" ~ "Hematology and Oncology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology Gynecology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology Maternal and Fetal Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology Obstetrics" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Obstetrics and Gynecology Reproductive Endocrinology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Ophthalmic Plastic and Reconstructive Surgery" ~ NA_character_, 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Ophthalmology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Ophthalmology Dental Providers Dentist" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Ophthalmology Dental Providers Dentist Oral and Maxillofacial Surgery" ~ NA_character_, 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Ophthalmology Glaucoma Specialist"~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Oral and Maxillofacial Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Adult Reconstructive Orthopedic Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Foot and Ankle Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Hand Surgery" ~ NA_character_, 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Orthopedic Surgery of the Spine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Orthopedic Trauma" ~ NA_character_, 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Orthopedic Surgery Sports Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology" ~ NA_character_, 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Facial Plastic Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Otolaryngic Allergy" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Otology & Neurotology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Pediatric Otolaryngology" ~ NA_character_, 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Plastic Surgery within the Head and Neck" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pain Medicine Interventional Pain Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pain Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Anatomic Pathology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Anatomic Pathology and Clinical Pathology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Clinical Pathology" ~ NA_character_, 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Clinical Pathology Laboratory Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Cytopathology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Dermapathology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Hematology" ~ NA_character_, 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Immunopathology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Molecular Genetic Pathology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Neuropathology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics"  ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Adolescent Medicine" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Developmental–Behavioral Pediatrics" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Neonatal-Perinatal Medicine" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Neurodevelopmental Disabilities" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Allergy and Immunology" ~ "Allergy and Immunology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Cardiology" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatric Critical Care Medicine" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Emergency Medicine" ~ "Hosptial",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Endocrinology" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatric Gastroenterology" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Hematology-Oncology" ~ "Hematology and Oncology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Infectious Diseases" ~ "Infectious Disease",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Nephrology" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Pulmonology" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Rheumatology" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Transplant Hepatology" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Sleep Medicine" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation Hospice and Palliative Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation Neuromuscular Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation Pain Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Physical Medicine and Rehabilitation Pediatric Rehabilitation Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Physical Medicine and Rehabilitation Spinal Cord Injury Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Physical Medicine and Rehabilitation Sports Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Physicians Plastic Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Plastic Surgery Surgery of the Hand" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Preventive Medicine Public Health and General Preventive Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Preventive Medicine Undersea and Hyperbaric Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Child and Adolescent Psychiatry" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Clinical Neurophysiology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Diagnostic Neuroimaging" ~NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Neurocritical Care" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Neurology" ~ "Neurology", 
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Neurology with Special Qualifications in Child Neurology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Neuromuscular Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Pain Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Sleep Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Psychiatry and Neurology Vascular Neurology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Radiology Diagnostic Radiology" ~ "Radiology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Radiology Radiation Oncology" ~ "Hematology and Oncology",
                            provider_taxonomy_desc == "Radiology Vascular and Interventional Radiology" ~ "Radiology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery" ~ "Hospital",
                            provider_taxonomy_desc ==  "Allopathic and Osteopathic Physicians Pediatrics Surgery Pediatric Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery Plastic and Reconstructive Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery Surgery of the Hand" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery Surgical Critical Care" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery Surgical Oncology" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Surgery Vascular Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Thoracic Surgery (Cardiothoracic Vascular Surgery)" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Transplant Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Urology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Urology Pediatric Urology" ~ NA_character_,
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic-Center Ambulatory Surgical" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic-Center Radiology Mammography" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic-Center Radiology Mobile Mammography" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Mental Health" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Multi-Specialty" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Physical Therapy" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Radiology" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Rehabilitation" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Rehabilitation Comprehensive Outpatient Rehabilitation Facility (CORF)" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Clinic Center Rural Health" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities End-Stage Renal Disease (ESRD) Treatment" ~ "Hospital",
                            provider_taxonomy_desc == "Ambulatory Health Care Facilities Federally Qualified Health Center (FQHC)" ~ "Hospital",
                            provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist" ~ NA_character_,
                            provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Addiction (Substance Abuse Disorder)"~ NA_character_,
                            provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Clinical" ~ NA_character_,
                            provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Clinical Child and Adolescent" ~ NA_character_,
                            provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Cognitive and Behavioral" ~ NA_character_,
                            provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Counseling" ~ NA_character_,
                            provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Health" ~ NA_character_,
                            provider_taxonomy_desc == "Behavioral Health and Social Service Providers Social Worker Clinical" ~ NA_character_,
                            provider_taxonomy_desc == "Chiropractic Providers Chiropractor" ~ NA_character_,
                            provider_taxonomy_desc == "Chiropractic Providers Chiropractor Occupational Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Chiropractic Providers Chiropractor Rehabilitation" ~ NA_character_,
                            provider_taxonomy_desc == "Dietary and Nutritional Service Providers Dietician Registered" ~ NA_character_,
                            provider_taxonomy_desc == "Dietary and Nutritional Service Providers Dietician Registered Nutrition Pediatric" ~ NA_character_,
                            provider_taxonomy_desc == "Eye and Vision Service Providers Optometrist" ~ NA_character_,
                            provider_taxonomy_desc == "Eye and Vision Service Providers Optometrist Corneal and Contact Management" ~ NA_character_,
                            provider_taxonomy_desc == "Eye and Vision Service Providers Technician Technologist Optician" ~ NA_character_,
                            provider_taxonomy_desc == "Female Pelvic Medicine and Reconstructive Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Group Single-Specialty" ~ NA_character_,
                            provider_taxonomy_desc == "Hospital Units Medicare Defined Swing Bed Unit" ~ "Hospital",
                            provider_taxonomy_desc == "Hospital Units Psychiatric Unit" ~ "Hospital",
                            provider_taxonomy_desc == "Hospital Units Rehabilitation Unit" ~ "Hospital",
                            provider_taxonomy_desc == "Hospitals General Acute Care Hospital Children" ~ "Hospital",
                            provider_taxonomy_desc == "Hospitals General Acute Care Hospital Critical Access" ~ "Hospital",
                            provider_taxonomy_desc == "Hospitals Long Term Care Hospital" ~ "Hospital",
                            provider_taxonomy_desc == "Hospitals Psychiatric Hospital" ~ "Hospital",
                            provider_taxonomy_desc == "Hospitals Rehabilitation Hospital" ~ "Hospital",
                            provider_taxonomy_desc == "Hospitals Special(ty) Hospital" ~ "Hospital",
                            provider_taxonomy_desc == "Internal Medicine Hospice and Palliative Medicine" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Laboratories Clinical Medical Laboratory" ~ "Laboratory",
                            provider_taxonomy_desc == "Laboratories Physiological Laboratory" ~ "Laboratory",
                            provider_taxonomy_desc == "Nursing and Custodial Care Facilities Nursing Facility" ~ "Nursing Facility",
                            provider_taxonomy_desc == "Nursing and Custodial Care Facilities Skilled Nursing Facility" ~ "Nursing Facility",
                            provider_taxonomy_desc == "Physician Assistants  and Advanced Practice Nursing Providers Clinical Nurse Specialist" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Acute Care" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Adult Health" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Family Health" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Oncology" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Oncology Pediatrics" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist Pediatrics" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Anesthetist Certified Registered" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == 'Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner' ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Acute Care"  ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Adult Health" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Family" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Gerontology" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Neonatal" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Obstetrics and Gynecology" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Pediatrics" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Perinatal" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Primary Care" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Psychiatric Mental Health" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Women’s Health" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Physician Assistant" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Physician Assistant Medical" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Physician Assistant Surgical" ~ "Hospital",
                            provider_taxonomy_desc == "Podiatric Medicine and Surgery Service Providers Podiatrist" ~ NA_character_,
                            provider_taxonomy_desc == "Podiatric Medicine and Surgery Service Providers Podiatrist Foot and Ankle Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Podiatric Medicine and Surgery Service Providers Podiatrist Primary Podiatric Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Respiratory Developmental Rehabilitative and Restorative Service Providers Occupational Therapist" ~ NA_character_,
                            provider_taxonomy_desc == "Respiratory Developmental Rehabilitative and Restorative Service Providers Occupational Therapist Feeding Eating &Swallowing" ~ NA_character_,
                            provider_taxonomy_desc == "Respiratory Developmental Rehabilitative and Restorative Service Providers Occupational Therapist Hand" ~ NA_character_,
                            provider_taxonomy_desc == "Respiratory Developmental Rehabilitative and Restorative Service Providers Occupational Therapist Pediatrics" ~ NA_character_,
                            provider_taxonomy_desc == "Respiratory Developmental Rehabilitative and Restorative Service Providers Physical Therapist" ~ NA_character_,
                            provider_taxonomy_desc == "Respiratory Rehabilitative and Restorative Service Providers Physical Therapist Sports" ~ NA_character_,
                            provider_taxonomy_desc == "Speech Language and Hearing Service Providers" ~ NA_character_,
                            provider_taxonomy_desc == "Speech Language and Hearing Service Providers Audiologist" ~ NA_character_,
                            provider_taxonomy_desc == "Transportation Services Ambulance" ~ NA_character_,
                            provider_taxonomy_desc == "Transportation Services Ambulance Air Transport" ~ NA_character_,
                            provider_taxonomy_desc == "Transportation Services Ambulance Land Transport" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Emergency Medicine" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Family Medicine Sports Medicine"  ~ "Family Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Internal Medicine Geriatric Medicine" ~ "Internal Medicine",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Nuclear Medicine Nuclear Cardiology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Otolaryngology/Facial Plastic Surgery" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Otolaryngology Otology &Neurotology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pain Medicine Pain Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pathology Blood Banking and Transfusion Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Adolescent Medicine" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Critical Care Medicine"  ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Pediatrics Pediatric Gastroenterology" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation Pediatric Rehabilitation Medicine" ~ "Pediatrics",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Physical Medicine and Rehabilitation Sports Medicine" ~ NA_character_,
                            provider_taxonomy_desc ==  "Allopathic and Osteopathic Physicians Plastic Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Plastic Surgery Plastic Surgery Within the Head and Neck" ~"Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Plastic Surgery Surgery of the Hand" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Preventive Medicine  Preventive Medicine Occupational Environmental Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Preventive Medicine Public Health and General Preventive Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Preventive Medicine Undersea and Hyperbaric Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology" ~ "Neurology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Child and Adolescent Psychiatry" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Clinical Neurophysiology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Diagnostic Neuroimaging" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Neurocritical Care" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Neurology" ~ "Neurology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Neurology with Special Qualifications in Child Neurology" ~ "Neurology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Neuromuscular Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Pain Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Psychosomatic Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Sleep Medicine" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Psychiatry and Neurology Vascular Neurology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Radiology Diagnostic Radiology" ~ "Radiology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Radiology Radiation Oncology" ~ "Radiology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Radiology Vascular and Interventional Radiology" ~ "Radiology",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Pediatric Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Plastic and Reconstructive Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Surgery of the Hand" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Surgical Critical Care" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Surgical Oncology" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Surgery Vascular Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Thoracic Surgery (Cardiothoracic Vascular Surgery)" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Transplant Surgery" ~ "Hospital",
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Urology" ~ NA_character_,
                            provider_taxonomy_desc == "Allopathic and Osteopathic Physicians Urology Pediatric Urology" ~ "Pediatrics",
                            provider_taxonomy_desc == "Behavioral Health and Social Service Providers Psychologist Prescribing (Medical)" ~ NA_character_,
                            provider_taxonomy_desc == "Chiropractic Providers Chiropractor Orthopedic" ~ NA_character_,
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Clinical Nurse Specialist" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Midwife Certified Nurse" ~ "Physician Assistant and Advanced Practice Nursing",
                            provider_taxonomy_desc == "Physician Assistants and Advanced Practice Nursing Providers Nurse Practitioner Pediatrics Critical Care" ~ "Physician Assistant and Advanced Practice Nursing",
                            TRUE ~ provider_taxonomy_desc))

xtabs(med2$NPIcollapsed, med2$place_of_service)
