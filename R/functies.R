# read_data <- function(file){
#   
#   read_excel(file)
#   
# }


# data <- read_data("testdata/data met alle kolommen.xls")


conversiefunctie <- function(data){
  
  data %>% 
    select(selectie) %>% 
    mutate(Meetobject.Namespace = "NLBW39", .before = Meetobject.lokaalid) %>% 
    filter(Parameter.code %in% c("CHLFa", "E_COLI", "INTTNLETRCCN") | 
             Typering.code %in% c("CATCANBTRDLG", "CATCANBTRMT"),
           Hoedanigheid.code != "DNAKPN") %>% 
    filter(!is.na(Numeriekewaarde))
  
}

# schrijffunctie <- function(tabel){
#   tabel %>% 
#     write_delim(file = glue("{format(now(), '%Y-%m-%d %H%M')} import_zwemwaterportaal.csv"),
#                 delim = ";", na = "")
# }

missende_data <- function(data){
  
  data %>% 
    select(selectie) %>% 
    mutate(Meetobject.Namespace = "NLBW39", .before = Meetobject.lokaalid) %>% 
    filter(Parameter.code %in% c("CHLFa", "E_COLI", "INTTNLETRCCN") | 
             Typering.code %in% c("CATCANBTRDLG", "CATCANBTRMT"),
           Hoedanigheid.code != "DNAKPN") %>% 
    filter(is.na(Numeriekewaarde)) %>% 
    select(Meetobject.lokaalid, Typering.code, Parameter.code, Begindatum, Begintijd)
  
}

overschrijdingen <- function(data) {
  data %>% 
    conversiefunctie() %>% 
    filter(Parameter.code == "E_COLI" & Numeriekewaarde > 1800 |
           Parameter.code == "INTTNLETRCCN" & Numeriekewaarde > 400 |
           Hoedanigheid.code == "toxblauw" & Numeriekewaarde > 12 |
            Typering.code == "CATCANBTRDLG" & Numeriekewaarde == 3
           ) %>% 
    select(Meetobject.lokaalid, Typering.code, Parameter.code, Hoedanigheid.code, Begindatum, Begintijd, Numeriekewaarde, Eenheid.code)
    
}
#   
# data %>% 
#   overschrijdingen()
  
# 
# file <- "testdata/data met alle kolommen.xls"
# 
# file %>% 
#   read_excel() %>% 
#   conversiefunctie() %>% 
#   schrijffunctie()
#   
# 
# schrijffunctie



# data %>% select(all_of(selectie))
