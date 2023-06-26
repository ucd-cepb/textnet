library(stringr)

govscitbl <- readRDS("data/govscienceuseR_agencies.RDS")

govscitbl$Agency <- str_remove(govscitbl$Agency, '^The\\s')
govscitbl$Agency <- str_remove(govscitbl$Agency, '^California\\s')
govscitbl$Agency <- str_remove(govscitbl$Agency, '^United\\sStates\\s')

govscitbl$Agency <- str_remove(govscitbl$Agency, '^(US\\s|U\\.S\\.\\s){1,}')

#kings-custom filter for California and fed only
govscitbl <- govscitbl[govscitbl$State %in% c("federal", "California"),]

#list below adapted from govscienceuseR > referenceExtract > clean_functions.R
#govscienceuseR Development Team 
#(Tyler A. Scott, Liza Wood, and Arthur Koehl) 
#(2022-2023). govscienceuseR: 
#Tools for automated extraction and disambiguation of 
#scientific resources cited in government documents. 
#govscienceuseR.github.io

org_words <- c("Administration", "Agency", "Association", "Associates", "Authority",  
               "Board", "Bureau", "Center", "^Consult[a-z]+$",
               "Commission", "Council", "County",  "Department", "Datacenter", "District", 
               "Foundation", "Government[s]*", "Group", 
               "Institute", "LLC", "Laboratory", "Office", "Service", "Society", "Survey",  
               "Univeristy")
org_phrases <- as.vector(outer(org_words, c("of","on","for"), paste, sep = " "))
org_phrases <- paste(org_phrases, collapse = "|")

spl <- strsplit(govscitbl$Agency, ',\\s*')

orgs_reordered <- sapply(1:length(spl), function (x) {
  #if one of splits (opt: besides the first one) has an org word followed by "of" or "for" 
  if(sum(grepl(org_phrases, spl[[x]]))>0){
    #then move all of the following splits to the beginning:
    #find first match: grep(org_phrases, x)[1]
    #and put all the elements after that at the front of the vector
    sorted <- c(spl[[x]][grep(org_phrases, spl[[x]])[1]:length(spl[[x]])], spl[[x]][-(grep(org_phrases, spl[[x]])[1]:length(spl[[x]]))])
    #combine the first two elements so there's no comma after "of" / "for" / "on"
    if(length(sorted)>1){
      sorted <- c(paste(sorted[1],sorted[2]), sorted[-(1:2)])
    }
    sorted
  }else{spl[[x]]
  }
})


orgs_with_commas <- sapply(orgs_reordered, function (x) {paste(x, collapse = ", ")})

orgs_underscore <- sapply(strsplit(orgs_with_commas,',*\\s+'), function(x) paste(x, collapse = "_"))

govscitbl$Agency <- orgs_underscore

#only keep rows where agency name is not an acronym. we match acronyms later in this script.
#This is case sensitive! which is why tolower is later in the script
govscitbl <- govscitbl[-grep(pattern = "(\\b*[A-Z]+\\b*)", x = govscitbl$Agency),]

#sort alphabetically
govscitbl <- govscitbl[order(govscitbl$Agency),]
#removing existing abbrevs and unofficial names from agencies. We will move these to abbrev column.
govscitbl <- govscitbl[!(govscitbl$Agency %in% c("Agriculture_Department",
                                                 "Archives_National_Archives_and_Records_Administration",
                                                 "Bureau_of_Alcohol_and_Tobacco_Tax_and_Trade",
                                                 "Alcohol_Tobacco_Firearms_and_Explosives_Bureau",
                                                 "Bureau_of_Consumer_Financial_Protection",
                                                 "Bureau_of_the_Census",
                                                 "Energy_Commission",
                                                 "Governor‚Äôs_Office_of_Business_and_Economic_Development",
                                                 "Office_of_Statewide_Health_Planning_and_Development",
                                                 "CDC",
                                                 "Commerce_Department",
                                                 "Consumer Services, and Housing Agency California Business",
                                                 "Defense_Department",
                                                 "Energy_Department",
                                                 "Education_Department",
                                                 "Fair_Housing_and_Equal_Opportunity",
                                                 "Fannie_Mae",
                                                 "Freddie_Mac",
                                                 "Ginnie_Mae",
                                                 "Health_and_Human_Services_Department",
                                                 "Homeland_Security_Department",
                                                 "Indian_Affairs",
                                                 "Interior_Department",
                                                 "Interpol",
                                                 "Justice_Department",
                                                 "Kennedy_Center",
                                                 "Labor_Department",
                                                 "NASA",
                                                 "Archives_National_Archives_and_Records_Administration",
                                                 "National_Library_of_Agriculture",
                                                 "NOAA_Fisheries",
                                                 "Northwest_Power_Planning_Council",
                                                 "NRC",
                                                 "Office_for_Civil_Rights_Department_of_Health_and_Human_Services",
                                                 "Open_World_Leadership_Center",
                                                 "Presidential_Scholars_Commission",
                                                 "Prisoner_of_War_and_Missing_in_Action_Accounting_Agency",#TODO check slash
                                                 "Science_Office",
                                                 "Treasury_Department",
                                                 "Veterans_Affairs_Department",
                                                 "and_Geologists_Board_of_Professional_Engineers_Land_Surveyors",
                                                 "and_Suisun_Board_of_Pilot_Commissioners_for_the_Bays_of_San_Francisco_San_Pablo",
                                                 "House_of_Representatives",
                                                 "Senate"
                                                 
)
),]


#abbrevs for unique california and federal orgs/agencies
#TODO perhaps move to govscienceuseR?
customabbr <- rbind(
  c("federal","Administration_for_Children_and_Families", "ACF"),
  c("federal","Congress—US_Senate","Senate"),#because "California State Senate" goes to "State Senate" and "US Senate" goes to "Senate" with the US/Cal prefix drop
  c("federal","Congress—US_House_of_Representatives","House_of_Representatives"),
  c("federal","Administration_for_Community_Living","ACL"),
  c("federal","Administrative_Conference_of_the_United_States","ACUS"),
  c("federal","Administrative_Office_of_the_US_Courts","AOUSC"),
  c("federal","Advisory_Council_on_Historic_Preservation","ACHP"),
  c("federal","Agency_for_Global_Media","USAGM"),
  c("federal","Agency_for_Healthcare_Research_and_Quality","AHRQ"),
  c("federal","Agency_for_International_Development","USAID"),
  c("federal","Agency_for_Toxic_Substances_and_Disease_Registry","ATSDR"),
  c("federal","Alcohol_and_Tobacco_Tax_and_Trade_Bureau","TTB"),
  c("federal","Alcohol_and_Tobacco_Tax_and_Trade_Bureau","Bureau_of_Alcohol_and_Tobacco_Tax_and_Trade"),
  c("federal","Bureau_of_Alcohol_Tobacco_Firearms_and_Explosives","Alcohol_Tobacco_Firearms_and_Explosives_Bureau"),
  c("federal","Bureau_of_Alcohol_Tobacco_Firearms_and_Explosives","ATF"),
  c("federal","Animal_and_Plant_Health_Inspection_Service","APHIS"),
  c("federal","Animal_and_Plant_Health_Inspection_Service","USDA_APHIS"),
  c("federal","Animal_and_Plant_Health_Inspection_Service","USDAAPHIS"),
  c("federal","National_Archives_and_Records_Administration","Archives_National_Archives_and_Records_Administration"),
  c("federal","National_Archives_and_Records_Administration","NARA"),
  c("federal","National_Archives_and_Records_Administration","National_Archives"),
  c("federal","Center_for_Legislative_Archives",NA),
  c("federal","Consumer_Financial_Protection_Bureau","Bureau_of_Consumer_Financial_Protection"),
  c("federal","Consumer_Financial_Protection_Bureau","CFPB"),
  c("federal","Bureau_of_Economic_Analysis","BEA"),#before tolower
  c("federal","Bureau_of_Economic_Analysis","USBEA"),
  c("federal","Bureau_of_Justice_Statistics","BJS"),
  c("federal","Bureau_of_Land_Management","BLM"),
  c("federal","Bureau_of_Land_Management","USBLM"),
  c("federal","Bureau_of_Labor_Statistics","BLS"),
  c("federal","Bureau_of_Labor_Statistics","USBLS"),
  c("federal","Bureau_of_Ocean_Energy_Management","BOEM"),
  c("federal","Bureau_of_Reclamation","USBOR"),
  c("federal","Bureau_of_Reclamation","USBR"),
  c("federal","Bureau_of_Safety_and_Environmental_Enforcement","BSEE"),
  c("federal","Census_Bureau","Bureau_of_the_Census"),
  c("federal","Census_Bureau","USCB"),
  c("California","Agricultural_Labor_Relations_Board", "ALRB"),
  c("California","Agricultural_Labor_Relations_Board","Agricultural_Labor_Relations_Board"),
  c("California","Air_Resources_Board","CARB"),#before tolower
  c("California","Alcoholic_Beverage_Control_Appeals_Board","ABCAB"),
  c("California","Alcoholic_Beverage_Control_Appeals_Board","ABC_Appeals_Board"),
  c("California","Alternative_Energy_and_Advanced_Transportation_Financing_Authority","CAEATFA"),
  c("California","Bureau_of_Household_Goods_and_Services","BHGS"),
  c("California","Central_Valley_Flood_Protection_Board","CVFPB"),
  c("California","Commission_on_Health_and_Safety_and_Workers'_Compensation","CHSWC"),
  c("California","Council_on_Criminal_Justice_and_Behavioral_Health","CCJBH"),
  c("California","Debt_Limit_Allocation_Committee","CDLAC"),
  c("California","Department_of_Child_Support_Services","DCSS"),#can have local chapters called the same thing
  c("California","Department_of_Corrections_and_Rehabilitation", "CDCR"),
  c("California","Department_of_FI$Cal","FI$Cal"),
  c("California","Department_of_Financial_Protection_and_Innovation","DFPI"),
  c("California","Department_of_Fish_and_Wildlife","CDFW"),
  c("California","Department_of_Food_and_Agriculture","CDFA"),
  c("California","Department_of_Forestry_and_Fire_Protection","CAL FIRE"),
  c("California","Department_of_Forestry_and_Fire_Protection","CALFIRE"),
  c("California","Department_of_Human_Resources","CalHR"),
  c("California","Department_of_Managed_Health_Care","DMHC"),
  c("California","Department_of_Motor_Vehicles","DMV"),#all states have one of these
  c("California","Department_of_Parks_and_Recreation","California_State_Parks"),
  c("California","Department_of_Pesticide_Regulation","CDPR"),
  c("California","Department_of_Pesticide_Regulation","DPR"),#also a construction company
  c("California","Department_of_Public_Health","CDPH"),
  c("California","Department_of_Rehabilitation","DOR"),#other states have dor that means something different
  c("California","Department_of_Resources_Recycling_and_Recovery","CalRecycle"),
  c("California","Department_of_Tax_and_Fee_Administration","CDTFA"),
  c("California","Department_of_Toxic_Substances_Control","DTSC"),
  c("California","Department_of_Transportation","Caltrans"),
  c("California","Department_of_Veterans_Affairs","CalVet"),
  c("California","Department_of_Water_Resources","DWR"),
  c("California","Disabled_Veterans_Business_Enterprise_Advisory_Council", "DVBE_Advisory_Council"),
  c("California","Emergency_Medical_Services_Authority","EMSA"),#this also stands for European Maritime Safety Agency
  c("California","Energy_Resources_Conservation_and_Development_Commission","California_Energy_Commission"),
  c("California","Environmental_Protection_Agency","CalEPA"),
  c("California","Exposition_and_State_Fair","Cal Expo"),
  c("California","Fair_Political_Practices_Commission","FPPC"),
  c("California","Financing_Coordinating_Committee","CFCC"),#also a college in North Carolina
  c("California","Franchise_Tax_Board","FTB"),
  c("California","Government_Operations_Agency","GovOps"),
  c("California","Governor's_Office_of_Emergency_Services", "Cal OES"),
  c("California","Governor's_Office_of_Planning_and_Research", "Cal OPR"),#also called OPR but DOJ's Office of Prof. Resp. is also abbreviated OPR
  c("California","Governor's_Office_of_Business_and_Economic_Development", "GO-Biz"),
  c("California","Health_and_Human_Services_Agency","CalHHS"),
  c("California","Health_Facilities_Financing_Authority","CHFFA"),
  c("California","Healthy_Food_Financing_Initiative_Council", "CHFFIC"),
  c("California","Highway_Patrol","CHP"),#also community housing partners, in VA
  c("California","Historical_Records_Advisory_Board","CHRAB"),
  c("California","Housing_Finance_Agency","CalHFA"),
  c("California","Industrial_Development_Financing_Advisory_Commission","CIDFAC"),
  c("California","Infrastructure_and_Economic_Development_Bank", "CIEDB"),
  c("California","Legislative_Analyst's_Office","LAO"),
  c("California","Mental_Health_Services_Oversight_and_Accountability_Commission","MHSOAC"),
  c("California","Board_for_Professional_Engineers_Land_Surveyors_and_Geologists",NA),
  c("California","Board_of_Pilot_Commissioners_for_the_Bays_of_San_Francisco_San_Pablo_and_Suisun","Board_of_Pilot_Commissioners"),#BOPC but shares acronym with other orgs/programs
  #Cal Guard vs Calif. State Guard vs Calif National Guard vs Calif. Military Dept??
  c("California","Native_American_Heritage_Commission","NAHC"),#also stands for Natl. Assn. for Home Care and Hospice
  c("California","Natural_Resources_Agency","CNRA"),#also Calif. North Referee Administration for soccer
  c("California","Ocean_Protection_Council","OPC"),#also a company and a religious org
  c("California","Office_of_Administrative_Hearings","OAH"),#also org of american historians
  c("California","Office_of_Administrative_Law",""),
  c("California","Office_of_Digital_Innovation","ODI"),
  c("California","Office_of_Environmental_Health_Hazard_Assessment","OEHHA"),
  c("California","Center_for_Data_Insights_and_Innovation","CalOHII"),#old acronym
  c("California","Center_for_Data_Insights_and_Innovation","California_Office_of_Health_Information_Integrity"),#old name
  c("California","Center_for_Data_Insights_and_Innovation","CDII"),
  c("California","Center_for_Data_Insights_and_Innovation","CalCDII"),
  c("California","Department_of_Health_Care_Access_and_Information","HCAI"),
  c("California","Department_of_Health_Care_Access_and_Information","California_Office_of_Statewide_Health_Planning_and_Development"),#previous name
  c("California","Department_of_Health_Care_Access_and_Information","OSHPD"),
  #OSI is the name of a CA company and CA gov office
  c("California","Office_of_the_State_Fire_Marshal","OSFM"),
  c("California","Pollution_Control_Financing_Authority","CPCFA"),
  c("California","Prison_Industry_Authority","CalPIA"),
  c("California","Prison_Industry_Authority","CALPIA"),
  c("California","Public_Employees_Retirement_System","CalPERS"),
  c("California","Public_Employment_Relations_Board","PERB"),#many states have one of these
  c("California","Public_Utilities_Commission","CPUC"),
  c("California","San_Francisco_Bay_Conservation_and_Development_Commission","BCDC"),
  c("California","San_Francisco_Bay_Conservation_and_Development_Commission","Bay_Conservation_and_Development_Commission"),
  c("California","State_Council_on_Developmental_Disabilities","SCDD"),
  c("California","State_Historical_Resources_Commission","SHRC"),
  c("California","State_Mining_and_Geology_Board","SMGB"),
  c("California","State_Parks_and_Recreation_Commission","SPRC"),#also a national mental health center
  c("California","State_Teachers_Retirement_System","CalSTRS"),
  c("California","State_Transportation_Agency","CalSTA"),
  c("California","State_Water_Resources_Control_Board","SWRCB"),
  c("California","State_Water_Resources_Control_Board","State_Water_Board"),
  c("California","Tax_Credit_Allocation_Committee","CTCAC"),
  c("California","Veterans_Board",NA),
  c("California","Veterinary_Medical_Board","VMB"),
  c("California","Victim_Compensation_Board","CalVCB"),
  c("California","Worker's_Compensation_Appeals_Board","WCAB"),
  c("California","Workforce_Development_Board","CWDB"),
  c("federal","Center_for_Food_Safety_and_Applied_Nutrition","CFSAN"),
  c("federal","Center_for_Nutrition_Policy_and_Promotion","CNPP"),
  c("federal","Center_for_Parent_Information_and_Resources","CPIR"),
  c("federal","Centers_for_Medicare_and_Medicaid_Services","CMS"),
  c("federal","Central_Intelligence_Agency","CIA"),
  c("federal","Chemical_Safety_Board","CSB"),
  c("federal","Chief_Acquisition_Officers_Council","CAO_Council"),
  c("federal","Chief_Financial_Officers_Council","CFO_Council"),
  c("federal","Chief_Human_Capital_Officers_Council","CHCO_Council"),
  c("federal","Chief_Information_Officers_Council","CIO_Council"),
  #csac means too many things
  c("federal","Citizenship_and_Immigration_Services","UCSIS"),
  c("federal","Coast_Guard","USCG"),
  c("federal","Copyright_Office","USCOP"),
  c("federal","Department_of_Commerce","Commerce_Department"),
  c("federal","Commission_on_International_Religious_Freedom","USCIRF"),
  c("federal","Commission_on_Security_and_Cooperation_in_Europe","CSCE"),
  c("federal","Committee_on_Foreign_Investment_in_the_United_States","CFIUS"),
  c("federal","Commodity_Futures_Trading_Commission","CFTC"),
  c("federal","Community_Oriented_Policing_Services","COPS"),
  c("federal","Congressional_Budget_Office","CBO"),
  c("federal","Consumer_Product_Safety_Commission","CPSC"),
  c("California","Business_Consumer_Services_and_Housing_Agency","BCSH"),
  c("California","Physical Therapy Board of California","PTBC"),
  c("California","University_of_California","UC"),
  c("federal","Coordinating_Council_on_Juvenile_Justice_and_Delinquency_Prevention","CCJJDP"),
  c("federal","Army_Corps_of_Engineers","Corps_of_Engineers"),
  c("federal","Army_Corps_of_Engineers","ACE"),#before tolower!
  c("federal","Army_Corps_of_Engineers","ACOE"),
  c("federal","Army_Corps_of_Engineers","USACE"),
  c("federal","Army_Corps_of_Engineers","USACOE"),
  c("federal","Council_of_the_Inspectors_General_on_Integrity_and_Efficiency","CIGIE"),
  c("federal","Council_on_Environmental_Quality","CEQ"),
  c("federal","Court_Services_and_Offender_Supervision_Agency_for_the_District_of_Columbia","CSOSA"),
  c("federal","Customs_and_Border_Protection","CBP"),
  c("federal","Defense_Acquisition_University","DAU"),
  c("federal","Defense_Advanced_Research_Projects_Agency","DARPA"),
  c("federal","Defense_Contract_Audit_Agency","DCAA"),
  c("federal","Defense_Contract_Management_Agency","DCMA"),
  c("federal","Defense_Counterintelligence_and_Security_Agency","DCSA"),
  c("federal","Department_of_Defense","Defense_Department"),
  c("federal","Department_of_Defense","DOD"),
  c("federal","Department_of_Defense","DoD"),
  c("federal","Department_of_Defense","USDOD"),
  c("federal","Defense_Information_Systems_Agency","DISA"),
  c("federal","Defense_Nuclear_Facilities_Safety_Board","DNFSB"),
  c("federal","Defense_Technical_Information_Center","DTIC"),
  c("federal","Defense_Threat_Reduction_Agency","DTRA"),
  c("federal","Delaware_River_Basin_Commission","DRBC"),
  c("California","Dental_Hygiene_Board_of_California","DHBC"),
  c("federal","Department_of_Agriculture","USDA"),
  c("federal","Department_of_Agriculture","Agriculture_Department"),
  c("federal","Department_of_Education","Education_Department"),
  #dept of commerce to "doc" not ideal to disambiguate in a document
  c("federal","Department_of_Energy","DoE"),#dept of education is not shortened to doe
  c("federal","Department_of_Energy","DOE"),
  c("federal","Department_of_Energy","Energy_Department"),
  c("federal","Department_of_Health_and_Human_Services","HHS"),
  c("federal","Department_of_Health_and_Human_Services","USHHS"),
  c("federal","Department_of_Health_and_Human_Services","Health_and_Human_Services_Department"),
  c("federal","Department_of_Health_and_Human_Services_Office_for_Civil_Rights",NA),#there's also an OCR in the HHS dept. also used to denote pdf to word extraction
  c("federal","Department_of_Health_and_Human_Services_Office_for_Civil_Rights","Office_for_Civil_Rights_Department_of_Health_and_Human_Services"),
  c("federal","Department_of_Education_Office_for_Civil_Rights","Office_for_Civil_Rights_Department_of_Education"),
  c("federal","Department_of_Homeland_Security","DHS"),
  c("federal","Department_of_Homeland_Security","Homeland_Security_Department"),
  c("federal","Department_of_Housing_and_Urban_Development","HUD"),
  c("federal","Department_of_Justice","DoJ"),
  c("federal","Department_of_Justice","DOJ"),
  c("federal","Department_of_Justice","Justice_Department"),
  c("federal","Department_of_Labor","DoL"),
  c("federal","Department_of_Labor","DOL"),
  c("federal","Department_of_Labor","Labor_Department"),
  #dept of interior doi could get picked up in citations
  c("federal","Department_of_the_Interior","Interior_Department"),
  c("federal","Department_of_the_Interior","USDOI"),
  c("federal","Department_of_Transportation","DoT"),
  c("federal","Department_of_Transportation","DOT"),
  c("federal","Department_of_Transportation","USDOT"),
  c("federal","Department_of_Veterans_Affairs","Veterans_Affairs_Department"),
  #Veterans Affairs VA could get picked up as virginia
  c("federal","Drug_Enforcement_Administration","DEA"),
  c("federal","Economic_Research_Service","ERS"),
  c("federal","Election_Assistance_Commission","EAC"),#also an intergovernmental org in East Africa
  c("federal","Employee_Benefits_Security_Administration","EBSA"),
  c("federal","Environmental_Protection_Agency","EPA"),
  c("federal","Environmental_Protection_Agency","US_EPA"),
  c("federal","Environmental_Protection_Agency","USEPA"),
  c("federal","Equal_Employment_Opportunity_Commission","EEOC"),
  c("federal","Executive_Office_for_Immigration_Review","EOIR"),
  c("federal","Export-Import_Bank_of_the_United_States","EXIM"),#exim is also an email keyword
  c("federal","Office_of_Fair_Housing_and_Equal_Opportunity","FHEO"),
  c("federal","Office_of_Fair_Housing_and_Equal_Opportunity","Fair_Housing_and_Equal_Opportunity"),
  c("federal","Farm_Credit_System_Insurance_Corporation","FCSIC"),
  c("federal","Federal_Accounting_Standards_Advisory_Board","FASAB"),
  c("federal","Federal_Aviation_Administration","FAA"),
  c("federal","Federal_Bureau_of_Investigation","FBI"),
  c("federal","Federal_Deposit_Insurance_Corporation","FDIC"),
  c("federal","Federal_Election_Commission","FEC"),
  c("federal","Federal_Emergency_Management_Agency","FEMA"),
  c("federal","Federal_Energy_Regulatory_Commission","FERC"),
  c("federal","Federal_Financial_Institutions_Examination_Council","FFIEC"),
  c("federal","Federal_Geographic_Data_Committee","FGDC"),
  c("federal","Federal_Highway_Administration","FHWA"),
  c("federal","Federal_Housing_Administration","FHA"),
  c("federal","Federal_Home_Loan_Mortgage_Corporation","Freddie_Mac"),
  c("federal","Federal_Housing_Finance_Agency","FHFA"),
  c("federal","Federal_Labor_Relations_Authority","FLRA"),
  c("federal","Federal_Laboratory_Consortium_for_Technology_Transfer","FLC"),
  c("federal","Federal_Laboratory_Consortium_for_Technology_Transfer","Federal_Laboratory_Consortium"),
  c("federal","Federal_Law_Enforcement_Training_Center","FLETC"),
  c("federal","Federal_Mediation_and_Conciliation_Service","FMCS"),
  c("federal","Federal_Motor_Carrier_Safety_Administration","FMCSA"),
  c("federal","Federal_National_Mortgage_Association","Fannie_Mae"),
  c("federal","Federal_Railroad_Administration","FRA"),
  c("federal","Federal_Retirement_Thrift_Investment_Board","FRTIB"),
  c("federal","Federal_Student_Aid_Information_Center","FSAIC"),
  c("federal","Federal_Trade_Commission","FTC"),
  c("federal","Federal_Transit_Administration","FTA"),
  c("federal","Fish_and_Wildlife_Service","FWS"),#also federal work study
  c("federal","Fish_and_Wildlife_Service","USFWS"),
  c("federal","Fish_and_Wildlife_Service","US_FWS"),
  c("federal","Food_and_Drug_Administration","FDA"),
  c("federal","Food_and_Nutrition_Service","FNS"),
  c("federal","Food_Safety_and_Inspection_Service","FSIS"),
  c("federal","Foreign_Claims_Settlement_Commission","FCSC"),#also soccer
  c("federal","Forest_Service","USFS"),
  c("federal","Forest_Service","USDA_FS"),
  c("federal","Geological_Survey","USGS"),
  c("federal","Government_Accountability_Office","GAO"),
  c("federal","Government_National_Mortgage_Association","Ginnie_Mae"),
  c("federal","Grain_Inspection_Packers_and_Stockyards_Administration","GIPSA"),
  c("federal","Health_Resources_and_Services_Administration","HRSA"),
  c("federal","Immigration_and_Customs_Enforcement","ICE"),#before tolower
  c("federal","Bureau_of_Indian_Affairs","BIA"),
  c("federal","Bureau_of_Indian_Affairs","Indian_Affairs"),
  c("federal","Federal_Interagency_Committee_on_Indoor_Air_Quality","CIAQ"),
  c(NA,"Indoor_Air_Quality","IAQ"),
  c("federal","Interagency_Alternative_Dispute_Resolution_Working_Group","Interagency_ADR_Working_Group"),
  c("federal","Interagency_Committee_for_the_Management_of_Noxious_and_Exotic_Weeds","FICMNEW"),
  c("federal","Internal_Revenue_Service","IRS"),
  c("federal","International_Criminal_Police_Organization","Interpol"),
  c("federal","International_Criminal_Police_Organization","INTERPOL"),
  c("federal","International_Criminal_Police_Organization","InterPol"),
  c("federal","International_Criminal_Police_Organization","National_Central_Bureau_-_Interpol"),
  c("federal","Japan-United_States_Friendship_Commission","JUSFC"),
  c("federal","John_F_Kennedy_Center_for_the_Performing_Arts","Kennedy_Center"),
  c("federal","Joint_Congressional_Committee_on_Inaugural_Ceremonies","JCCIC"),
  c("federal","Joint_Fire_Science_Program","JFSP"),
  c("federal","Joint_Program_Executive_Office_for_Chemical_and_Biological_Defense","JPEO-CBRND"),
  c("federal","Judicial_Panel_on_Multidistrict_Litigation","JPML"),
  c("federal","Maritime_Administration","MARAD"),
  c("federal","Merit_Systems_Protection_Board","MSPB"),
  c("federal","Military_Academy_West_Point","West_Point"),
  c("federal","Mine_Safety_and_Health_Administration","MSHA"),
  c("California","California_Division_of_Occupational_Safety_and_Health","Cal/OSHA"),#TODO make sure this slash doesn't mess stuff up
  c("federal","National_Aeronautics_and_Space_Administration","NASA"),
  c("federal","National_Agricultural_Statistics_Service","NASS"),
  c("federal","National_Agricultural_Statistics_Service","USDA_NASS"),
  c("federal","National_Cancer_Institute","NCI"),
  c("federal","National_Credit_Union_Administration","NCUA"),
  c("federal","National_Flood_Insurance_Program","NFIP"),
  c("federal","National_Geospatial-Intelligence_Agency","NGIA"),
  c("federal","National_Health_Information_Center","NHIC"),
  c("federal","National_Heart_Lung_and_Blood_Institute","NHLBI"),
  c("federal","National_Highway_Traffic_Safety_Administration","NHTSA"),
  c("federal","National_Indian_Gaming_Commission","NIGC"),
  c("federal","National_Institute_of_Deafness_and_Other_Communication_Disorders","NIDCD"),
  c("federal","National_Institute_of_Diabetes_and_Digestive_and_Kidney_Diseases","NIDDK"),
  c("federal","National_Institute_of_Food_and_Agriculture","NIFA"),#also nebraska investment finance authority
  c("federal","National_Institute_of_Justice","NIJ"),
  c("federal","National_Institute_of_Mental_Health","NIMH"),
  c("federal","National_Institute_of_Neurological_Disorders_and_Stroke","NINDS"),
  c("federal","National_Institute_of_Occupational_Safety_and_Health","NIOSH"),
  c("federal","National_Institute_of_Standards_and_Technology","NIST"),
  c("federal","National_Institutes_of_Health","NIH"),
  c("federal","National_Interagency_Fire_Center","NIFC"),
  c("federal","National_Labor_Relations_Board","NLRB"),
  c("federal","National_Agricultural_Library","National_Library_of_Agriculture"),
  c("federal","National_Marine_Fisheries_Service","NMFS"),
  c("federal","National_Nuclear_Security_Administration","NNSA"),
  c("federal","National_Oceanic_and_Atmospheric_Administration","NOAA"),
  c("federal","National_Park_Service","NPS"),#also stands for net promoter score
  c("federal","National_Prevention_Information_Network","NPIN"),
  c("federal","National_Science_Foundation","NSF"),
  c("federal","National_Security_Agency","NSA"),
  c("federal","National_Security_Council","NSC"),
  c("federal","National_Technical_Information_Service","NTIS"),#also refers to baseball roster
  c("federal","National_Telecommunications_and_Information_Administration","NTIA"),
  c("federal","National_Transportation_Safety_Board","NTSB"),
  c("federal","National_Weather_Service","NWS"),
  c("federal","Natural_Resources_Conservation_Service","NRCS"),
  c("federal","Natural_Resources_Conservation_Service","Soil_Conservation_Service"),#old name
  c("federal","Natural_Resources_Conservation_Service","USDA_NRCS"),
  c("federal","Natural_Resources_Conservation_Service","USDA-NRCS"),
  c("federal","National Marine Fisheries Service","NOAA_Fisheries"),
  c("federal","Northwest_Power_and_Conservation_Council","Northwest_Power_Planning_Council"),#old name. #old acronym and new acronym not distinguishable from other orgs
  c("federal","Nuclear_Regulatory_Commission","USNRC"),
  c("federal","Nuclear_Waste_Technical_Review_Board","NWTRB"),
  c("federal","Occupational_Safety_and_Health_Review_Commission","OSHRC"),
  c("federal","Office_of_Career_Technical_and_Adult_Education","OCTAE"),
  c("federal","Office_of_Disability_Employment_Policy","ODEP"),
  c("federal","Office_of_Elementary_and_Secondary_Education","OESE"),
  c("federal","Office_of_Fair_Housing_and_Equal_Opportunity","FHEO"),
  c("federal","Office_of_Justice_Programs","OJP"),
  c("federal","Office_of_Juvenile_Justice_and_Delinquency_Prevention","OJJDP"),
  c("federal","Office_of_Lead_Hazard_Control_and_Healthy_Homes","OLHCHH"),
  c("federal","Office_of_Management_and_Budget","OMB"),
  c("federal","Office_of_Management_and_Budget","Management_and_Budget_Office"),
  c("federal","Office_of_Manufactured_Housing_Programs","OMHP"),
  c("federal","Office_of_National_Drug_Control_Policy","ONDCP"),
  c("federal","Office_of_Natural_Resources_Revenue","ONRR"),
  c("federal","Office_of_Science_and_Technology_Policy","OSTP"),
  c("federal","Office_of_Scientific_and_Technical_Information","OSTI"),
  c("federal","Office_of_Special_Education_and_Rehabilitative_Services","OSERS"),
  c("federal","Office_of_Surface_Mining_Reclamation_and_Enforcement","OSMRE"),
  c("federal","Office_of_the_Director_of_National_Intelligence","ODNI"),
  c("federal","Office_of_the_Director_of_National_Intelligence","Office_of_Director_of_National_Intelligence"),
  c("federal","Office_on_Violence_Against_Women","OVW"),#also ohio wrestling
  c("federal","Congressional_Office_for_International_Leadership","Open_World_Leadership_Center"),
  c("federal","Patent_and_Trademark_Office","USPTO"),
  c("federal","Pension_Benefit_Guaranty_Corporation","PBGC"),
  c("federal","Pentagon_Force_Protection_Agency","PFPA"),
  c("federal","Pipeline_and_Hazardous_Materials_Safety_Administration","PHMSA"),
  c("federal","Postal_Service","USPS"),
  c("federal","President's_Council_on_Fitness_Sports_and_Nutrition","PCFSN"),
  c("federal","Commission_on_Presidential_Scholars","Presidential_Scholars_Commission"),
  c("federal","Defense POW/MIA Accounting Agency","Prisoner_of_War_and_Missing_in_Action_Accounting_Agency"),
  c("federal","Defense POW/MIA Accounting Agency","DPAA"),#TODO make sure slash doesn't break anything
  c("federal","Privacy_and_Civil_Liberties_Oversight_Board","PCLOB"),
  c("federal","Saint_Lawrence_Seaway_Development_Corporation","SLSDC"),
  c("federal","Saint_Lawrence_Seaway_Development_Corporation","Great_Lakes_Saint_Lawrence_Seaway_Development_Corporation"),
  c("federal","Saint_Lawrence_Seaway_Development_Corporation","Great_Lakes_St._Lawrence_Seaway_Development_Corporation"),
  c("federal","Saint_Lawrence_Seaway_Development_Corporation","St._Lawrence_Seaway_Development_Corporation"),
  c("federal","Office_of_Science","Science_Office"),
  c("federal","Securities_and_Exchange_Commission","SEC"),
  c("federal","Substance_Abuse_and_Mental_Health_Services_Administration","SAMHSA"),
  c("federal","Supreme_Court_of_the_United_States","SCOTUS"),
  c("federal","Supreme_Court_of_the_United_States","Supreme_Court"),
  c("federal","Susquehanna_River_Basin_Commission","SRBC"),
  c("federal","Tennessee_Valley_Authority","TVA"),
  c("federal","Transportation_Security_Administration","TSA"),
  c("federal","Department_of_the_Treasury","Treasury_Department"),
  c("federal","Department_of_the_Treasury","USDT"),#also the code for the crypto company Tether
  c("federal","Computer_Emergency_Readiness_Team","US-CERT"),
  c("federal","Army_Combined_Arms_Center","USACAC"),
  c("federal","Army_Environmental_Command","USAEC"),
  c("federal","USAGov",NA),
  c("federal","Army_Public_Health_Center","USAPHC"),
  c("federal","Army_Alaska","USARAK"),
  c("federal","Army_Reserve_Command","USARC"),
  c("federal","Agricultural_Marketing_Service","USDA_-_Agricultural_Marketing_Service"),
  c("federal","Agricultural_Marketing_Service","USDA_Agricultural_Marketing_Service"),
  c("federal","Agricultural_Marketing_Service","USDA_AMS"),
  c("federal","Agricultural_Marketing_Service","USDA-AMS"),
  c("federal","Agricultural_Marketing_Service","USDAAMS"),
  c("federal","Agricultural_Research_Service","USDA_-_Agricultural_Research_Service"),
  c("federal","Agricultural_Research_Service","USDA_Agricultural_Research_Service"),
  c("federal","Agricultural_Research_Service","USDA_ARS"),
  c("federal","Agricultural_Research_Service","USDA-ARS"),
  c("federal","Agricultural_Research_Service","USDAARS"),   
  c("federal","Economic_Research_Service","USDA_-_Economic_Research_Service"),
  c("federal","Economic_Research_Service","USDA_Economic_Research_Service"),
  c("federal","Economic_Research_Service","USDA_ERS"),
  c("federal","Economic_Research_Service","USDA-ERS"),
  c("federal","Economic_Research_Service","USDAERS"),     
  c("federal","Foreign_Agricultural_Service","USDA_-_Foreign_Agricultural_Service"),
  c("federal","Foreign_Agricultural_Service","USDA_Foreign_Agricultural_Service"),
  c("federal","Foreign_Agricultural_Service","USDA_FAS"),
  c("federal","Foreign_Agricultural_Service","USDA-FAS"),
  c("federal","Foreign_Agricultural_Service","USDAFAS"), 
  c("federal","USDA_Office_of_the_Chief_Economist","USDA_-_Office_of_the_Chief_Economist"),
  c("federal","USDA_Office_of_the_Chief_Economist","USDA_OCE"),
  c("federal","USDA_Office_of_the_Chief_Economist","USDA-OCE"),
  c("federal","USDA_Office_of_the_Chief_Economist","USDAOCE"), 
  c("federal","USDA_Wildlife_Services","USDA_-_Wildlife_Services"),
  c("federal","USDA_Wildlife_Services","USDA_Wildlife_Services"),
  c("federal","USDA_Wildlife_Services","USDA_WS"),
  c("federal","USDA_Wildlife_Services","USDA-WS"),
  c("federal","Voice_of_America","VoA"),
  c("federal","Voice_of_America","VOA"),
  c("federal","Wage_and_Hour_Division","WHD"),#also world humanitarian day
  c("California","Sustainable_Groundwater_Management_Act","SGMA"),
  #the remaining lines are all sgma/groundwater-specific acronyms
  c("California","Data_Exchange_Center","CDEC"),
  c("California","Irrigation_Management_Information_System","CIMIS"),
  c("California","Central_Valley_Regional_Water_Quality_Control_Board","CVRWQCB"),
  c("California","Division_of_Drinking Water","DDW"),
  c("local","Groundwater_Sustainability_Plan","GSP"),
  c("local","San_Joaquin_River_Restoration_Program","SJRRP"),
  c("federal","National_Cooperative_Soil_Survey_Geographic_Database","SSURGO"),
  c("federal","National_Cooperative_Soil_Survey_Geographic_Database","Soil_Survey_Geographic_Database"),
  c("local","Integrated_Regional_Water_Management_Plan","IRWMP"),
  c("local","Integrated_Regional_Water_Management","IRWM"),
  c(NA,"Precipitation-Elevation_Regressions_on_Independent_Slopes_Model","PRISM"),
  c("California","Geologic_Energy_Management_Division", "CalGEM"),
  c("California","Statewide_Groundwater_Elevation_Monitoring","CASGEM"),
  c(NA,"Groundwater-Dependent_Ecosystem", "Groundwater_Dependent_Ecosystem"),
  c(NA,"Groundwater-Dependent_Ecosystem", "groundwater_dependent_ecosystem"),
  c(NA,"Groundwater-Dependent_Ecosystem", "GDE"),
  c("local","Groundwater_Sustainability_Agency", "GSA"),
  c(NA,"Groundwater_Management_Plan", "GWMP"),
  c("California","Irrigated_Lands_Regulatory_Program", "ILRP"),
  c("local","Regional_Water_Quality_Control_Board", "RWQCB"),
  c("local","Central_Coast_Regional_Water_Quality_Control_Board","CCRWQCB"),
  c("local","North_Coast_Regional_Water_Quality_Control_Board","NCRWQCB"),
  c("California","Environmental_Data_Exchange_Network","CEDEN"),
  c("federal","National_Centers_for_Environmental_Information", "NCEI"),
  c("federal","National_Centers_for_Environmental_Information", "NOAA_NCEI"),
  c("federal","National_Centers_for_Environmental_Information", "National_Climatic_Data_Center"),#old name; 
  c("federal","National_Centers_for_Environmental_Information", "NCDC"),#old acronym; also hockey org, dance conservancy, criminal defense college
  c("federal","Safe_Drinking_Water_Information_System","SDWIS"),
  c("local","Association_of_Monterey_Bay_Area_Governments","AMBAG"),
  c("local","Central_Coast_Groundwater_Coalition","CCGC"),#also community child guidance clinic, color guard org
  c("California","Environmental_Quality_Act","CEQA"),
  c("federal","Comprehensive_Environmental_Response_Compensation_and_Liability_Act","CERCLA"),
  c("federal","Public_Land_Survey_System","PLSS"),
  c("California","Department_of_Conservation_Division_of_Oil_Gas_and_Geothermal_Resources","DOGGR"),
  c(NA,"Natural_Communities_Commonly_Associated_with_Groundwater","NCCAG"),
  c("federal","National_Environmental_Policy_Act","NEPA"),
  c("federal","National_Pollutant_Discharge_Elimination_System", "NPDES"),
  c("federal","National_Water_Information_System","NWIS"),
  c("federal","National_Water_Quality_Monitoring_Council", "NWQMC"),
  c("California","University_of_California_Water_Security_and_Sustainability_Research_Initiative","UCWSSRI"),
  c("California","University_of_California_Water_Security_and_Sustainability_Research_Initiative","UC_Water_Security_and_Sustainability_Research_Initiative"),
  c(NA,"Disadvantaged_Community","DAC"),
  c(NA,"Disadvantaged_Communities","DACs"),
  c("federal","Environmental_Quality_Incentives_Program","EQIP"),
  c(NA,"Interferometric_Synthetic_Aperture_Radar","InSAR"),#also autism research org
  c(NA,"Maximum_Contaminant_Level","MCL"),
  c("local","Northern_California_Water_Association","NCWA"),#also wrestling org
  c("local","Northeastern_California_Water_Association","NECWA"),#also new england coastal wildlife alliance
  c("local","Pacific_Gas_and_Electric","PG&E"),
  c("California","University_of_California_Cooperative_Extension","UCCE"),#also cisco contact center
  c("California","University_of_California_Cooperative_Extension","UCCE"),
  c("California","Groundwater_Ambient_Monitoring_and_Assessment","GAMA"),#before tolower
  c("federal","Climate_Reference_Network","USCRN")
  #several meanings:botanical, companies, and orgs, and last name
)

colnames(customabbr) <- names(govscitbl)
#TODO check on entries with apostrophes


govscitbl <- rbind(govscitbl, customabbr)

govscitbl$Abbr <- ifelse(is.na(govscitbl$Abbr),NA,
                         ifelse(nchar(govscitbl$Abbr) == 0, NA,
                                ifelse(substr(govscitbl$Abbr,1,1) != "\b" & substr(govscitbl$Abbr,nchar(govscitbl$Abbr),nchar(govscitbl$Abbr)) != "\b",paste0("\b",govscitbl$Abbr,"\b"),
                                       govscitbl$Abbr)))

library(dplyr)
govscitbl <- govscitbl %>% distinct()

noabbr <- govscitbl %>% filter(is.na(Abbr) | nchar(Abbr) == 0)
yesabbr <- govscitbl %>% filter(!is.na(Abbr) & nchar(Abbr) != 0)

#if there are rows with no abbr for orgs that do have valid abbrs, remove those rows
noabbr <- noabbr[!(noabbr$Agency %in% yesabbr$Agency),]
govscitbl <- rbind(yesabbr, noabbr)



govscitbl <- govscitbl[!(govscitbl$Agency=="Nuclear_Regulatory_Commission" & govscitbl$Abbr=="\bNRC\b"),]
#NRC is already included in abbrevs but another national org and a state org share the acronym

duplicate_check <- duplicated(na.omit(govscitbl$Abbr))
if(sum(duplicate_check)>0){
  stop("One or more abbreviations point to multiple agencies and cannot be disambiguated.")
}

calif_dupl <- govscitbl %>% filter(State=="California")
calif_dupl$Abbr <- paste0("\bCalifornia_",calif_dupl$Agency,"\b")

govscitbl <- rbind(govscitbl, calif_dupl)


