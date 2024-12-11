
cps <- read.csv("data/cps_00006.csv")

#save this code into a src folder named clean_cps.R
#you'll also save code called clean_acs.R...
#and hopefully multiple analysis files - one for each of the Y variables you choose to model.
#your analysis files can source clean_cps.R using the source() function
#each row of cps is an INDIVIDUAL within a family
cps <- cps %>%
  mutate(SEX = SEX - 1 , # Create dummy variables
         CHILD = ifelse(AGE < 18, 1, 0),
         ELDERLY = ifelse(AGE > 60, 1, 0), #NOTE DEFINITION
         BLACK = ifelse(RACE==200, 1, 0),
         HISPANIC = ifelse(HISPAN>0, 1, 0),
         EDUC = as.integer(EDUC %in% c(91,92,111,123,124,125)),
         EMP = as.integer(EMPSTAT %in% c(1,10,12)),
         MARRIED = as.integer(MARST %in% c(1,2)),
         COUNTY = as.factor(COUNTY))

#currently, one row of cps = one individual
#however, we want to make prediction on the family level
#aggregate to the family level - this is where we choose FAMILY-LEVEL traits
#that we want to calculate. For example, household size is equal to the
#number of rows for that family.
cps_data <- cps %>%
  group_by(CPSID = as.factor(CPSID)) %>%
  summarise(COUNTY = first(COUNTY),
            #family level weight
            weight = first(HWTFINL),
            #household size
            hhsize = n(),
            #Y variables - i.e., measures of hunger
            #see CPS website for details
            #FSSTATUS, etc. is the same for each member -just take first value for each family
            FSTOTXPNC_perpers = FSTOTXPNC/hhsize, # In per person terms
            FSSTATUS = first(FSSTATUS),
            FSSTATUSMD = first(FSSTATUSMD),
            FSFOODS = first(FSFOODS),
            FSWROUTY = first(FSWROUTY),
            FSBAL = first(FSBAL),
            FSRAWSCRA = first(FSRAWSCRA),
            FSTOTXPNC = first(FSTOTXPNC),
            FSSTATUS = first(FSSTATUS),
            FAMINC = first(FAMINC),
            #count of family members in various categories
            female = sum(SEX),
            hispanic = sum(HISPANIC),
            black= sum(BLACK),
            kids= sum(CHILD),
            elderly= sum(ELDERLY),
            education= sum(EDUC),
            married = sum(MARRIED),
            p.female=sum(SEX)/n(),
            p.hispanic=sum(HISPANIC)/n(),
            p.black=sum(BLACK)/n(),
            p.kids=sum(CHILD)/n(),
            p.elderly=sum(ELDERLY)/n()) %>% ungroup()

#each row of cps_data is a FAMILY
#note... we just calculated the number of people in each family that belong
#to the above groups. perhaps that isn't the best way? Would proportions be good
#in addition or instead of sums?!
#summary(cps_data) # see extremes for food security variables
#https://cps.ipums.org/cps-action/variables/search
cps_data <- cps_data %>%
  mutate(FSSTATUS = ifelse(FSSTATUS %in% c(98,99), NA, FSSTATUS),
         FSSTATUSMD = ifelse(FSSTATUSMD %in% c(98,99), NA, FSSTATUSMD),
         FSFOODS = ifelse(FSFOODS %in% c(98,99), NA, FSFOODS),
         FSWROUTY = ifelse(FSWROUTY %in% c(96,97,98,99), NA, FSWROUTY),
         FSBAL = ifelse(FSBAL %in% c(96,97,98,99), NA, FSBAL),
         FSRAWSCRA = ifelse(FSRAWSCRA %in% c(98,99), NA, FSRAWSCRA),#raw score
         FSTOTXPNC = ifelse(FSTOTXPNC %in% c(999), NA, FSTOTXPNC)) %>%
  mutate(FSSTATUS = ifelse(FSSTATUS > 1, 1, 0),
         FSSTATUSMD = ifelse(FSSTATUSMD > 1, 1, 0),
         FSFOODS = ifelse(FSFOODS > 1, 1, 0),
         FSWROUTY = ifelse(FSWROUTY > 1, 1, 0),#more missings
         FSBAL = ifelse(FSBAL > 1, 1, 0),
         FSRAWSCRA=ifelse(FSRAWSCRA > 1, 1, 0))

cps_data <- cps_data %>%
  mutate(
    FAMINC_clean = case_when(
      FAMINC == 100 ~ "Under $5,000",
      FAMINC == 110 ~ "Under $1,000",
      FAMINC == 111 ~ "Under $500",
      FAMINC == 112 ~ "$500 - 999",
      FAMINC == 120 ~ "$1,000 - 1,999",
      FAMINC == 121 ~ "$1,000 - 1,499",
      FAMINC == 122 ~ "$1,500 - 1,999",
      FAMINC == 130 ~ "$2,000 - 2,999",
      FAMINC == 131 ~ "$2,000 - 2,499",
      FAMINC == 132 ~ "$2,500 - 2,999",
      FAMINC == 140 ~ "$3,000 - 3,999",
      FAMINC == 141 ~ "$3,000 - 3,499",
      FAMINC == 142 ~ "$3,500 - 3,999",
      FAMINC == 150 ~ "$4,000 - 4,999",
      FAMINC == 200 ~ "$5,000 - 7,999",
      FAMINC == 210 ~ "$5,000 - 7,499",
      FAMINC == 220 ~ "$5,000 - 5,999",
      FAMINC == 230 ~ "$6,000 - 7,999",
      FAMINC == 231 ~ "$6,000 - 7,499",
      FAMINC == 232 ~ "$6,000 - 6,999",
      FAMINC == 233 ~ "$7,000 - 7,499",
      FAMINC == 234 ~ "$7,000 - 7,999",
      FAMINC == 300 ~ "$7,500 - 9,999",
      FAMINC == 310 ~ "$7,500 - 7,999",
      FAMINC == 320 ~ "$8,000 - 8,499",
      FAMINC == 330 ~ "$8,500 - 8,999",
      FAMINC == 340 ~ "$8,000 - 8,999",
      FAMINC == 350 ~ "$9,000 - 9,999",
      FAMINC == 400 ~ "$10,000 - 14,999",
      FAMINC == 410 ~ "$10,000 - 10,999",
      FAMINC == 420 ~ "$11,000 - 11,999",
      FAMINC == 430 ~ "$10,000 - 12,499",
      FAMINC == 440 ~ "$10,000 - 11,999",
      FAMINC == 450 ~ "$12,000 - 12,999",
      FAMINC == 460 ~ "$12,000 - 14,999",
      FAMINC == 470 ~ "$12,500 - 14,999",
      FAMINC == 480 ~ "$13,000 - 13,999",
      FAMINC == 490 ~ "$14,000 - 14,999",
      FAMINC == 500 ~ "$15,000 - 19,999",
      FAMINC == 510 ~ "$15,000 - 15,999",
      FAMINC == 520 ~ "$16,000 - 16,999",
      FAMINC == 530 ~ "$17,000 - 17,999",
      FAMINC == 540 ~ "$15,000 - 17,499",
      FAMINC == 550 ~ "$17,500 - 19,999",
      FAMINC == 560 ~ "$18,000 - 19,999",
      FAMINC == 600 ~ "$20,000 - 24,999",
      FAMINC == 700 ~ "$25,000 - 49,999",
      FAMINC == 710 ~ "$25,000 - 29,999",
      FAMINC == 720 ~ "$30,000 - 34,999",
      FAMINC == 730 ~ "$35,000 - 39,999",
      FAMINC == 740 ~ "$40,000 - 49,999",
      FAMINC == 800 ~ "$50,000 and over",
      FAMINC == 810 ~ "$50,000 - 74,999",
      FAMINC == 820 ~ "$50,000 - 59,999",
      FAMINC == 830 ~ "$60,000 - 74,999",
      FAMINC == 840 ~ "$75,000 and over",
      FAMINC == 841 ~ "$75,000 - 99,999",
      FAMINC == 842 ~ "$100,000 - 149,999",
      FAMINC == 843 ~ "$150,000 and over",
      FAMINC == 995 ~ "Missing",
      FAMINC == 996 ~ "Refused",
      FAMINC == 997 ~ "Don't know",
      FAMINC == 999 ~ "Blank"
    )
  )

# Used ChatGPT here to make sure FAMINC was read in as categorical
cps_data <- cps_data %>%
  mutate(FAMINC_clean = factor(FAMINC_clean, levels = c(
    "Under $5,000", "Under $1,000", "Under $500", "$500 - 999", 
    "$1,000 - 1,999", "$1,000 - 1,499", "$1,500 - 1,999", "$2,000 - 2,999", 
    "$2,000 - 2,499", "$2,500 - 2,999", "$3,000 - 3,999", "$3,000 - 3,499", 
    "$3,500 - 3,999", "$4,000 - 4,999", "$5,000 - 7,999", "$5,000 - 7,499", 
    "$5,000 - 5,999", "$6,000 - 7,999", "$6,000 - 7,499", "$6,000 - 6,999", 
    "$7,000 - 7,499", "$7,000 - 7,999", "$7,500 - 9,999", "$7,500 - 7,999", 
    "$8,000 - 8,499", "$8,500 - 8,999", "$8,000 - 8,999", "$9,000 - 9,999", 
    "$10,000 - 14,999", "$10,000 - 10,999", "$11,000 - 11,999", 
    "$10,000 - 12,499", "$10,000 - 11,999", "$12,000 - 12,999", 
    "$12,000 - 14,999", "$12,500 - 14,999", "$13,000 - 13,999", 
    "$14,000 - 14,999", "$15,000 - 19,999", "$15,000 - 15,999", 
    "$16,000 - 16,999", "$17,000 - 17,999", "$15,000 - 17,499", 
    "$17,500 - 19,999", "$18,000 - 19,999", "$20,000 - 24,999", 
    "$25,000 - 49,999", "$25,000 - 29,999", "$30,000 - 34,999", 
    "$35,000 - 39,999", "$40,000 - 49,999", "$50,000 and over", 
    "$50,000 - 74,999", "$50,000 - 59,999", "$60,000 - 74,999", 
    "$75,000 and over", "$75,000 - 99,999", "$100,000 - 149,999", 
    "$150,000 and over", "Missing", "Refused", "Don't know", "Blank"
  )))

str(cps_data)
summary(cps_data)

