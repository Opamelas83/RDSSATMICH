FILE <- read_table("~/Desktop/Analy_project/RDSSATMICH/Evaluate.OUT")
spec(FILE)
FILES <- FILE %>%
    select(EXCODE, TRNO, HWAMS, HWAMM)

FILES$LOCATION <- FILES$EXCODE
FILES$LOCATION[FILES$LOCATION%like%"UYAB"] <- "ABUJA"
FILES$LOCATION[FILES$LOCATION%like%"UYAG"] <- "AGO_OWU"
FILES$LOCATION[FILES$LOCATION%like%"UYIB"] <- "IBADAN"
FILES$LOCATION[FILES$LOCATION%like%"UYIK"] <- "IKENNE"
FILES$LOCATION[FILES$LOCATION%like%"UYMO"] <- "MOKWA"
FILES$LOCATION[FILES$LOCATION%like%"UYON"] <- "ONNE"
FILES$LOCATION[FILES$LOCATION%like%"UYUB"] <- "UBIAJA"
FILES$LOCATION[FILES$LOCATION%like%"UYUM"] <- "UMUDIKE"
FILES$LOCATION[FILES$LOCATION%like%"UYZA"] <- "ZARIA"

FILES <- FILES %>%
  select(LOCATION, TRNO, HWAMS, HWAMM) %>%
  rename(simulated=HWAMS, observed=HWAMM) %>%
  filter(TRNO <= 20 & observed > 1000 & simulated > 1000) %>%
  mutate(DIF = (abs(simulated-observed)))

RMSE = function(FILES){
  sqrt(mean((simulated-observed)^2))
}



CHOICE <- filter(FILES, TRNO == 9)

ggplot(CHOICE)  +
  geom_point(aes(x = observed, y = simulated, color = LOCATION)) + 
  scale_color_manual("LOCATION",
                     values = c("red", "#FFDD45", rgb(0.1,0.2,0.6),
                                "darkgreen", "grey80", "cyan",
                                "blue1", "burlywood1", "aquamarine",
                                "chocolate", "orange")) +
  geom_smooth(aes(x = observed, y = simulated), method = "lm")





FILES <- FILES %>%
  Filter(LOCATIONS, )

ABUJA <- FILES %>%
  filter(EXCODE == "UYAB1901CS") %>%
  mutate(MHWAMS=mean(HWAMS), MHWAMM=mean(HWAMM))
AGO_OWU <- FILES %>%
  filter(EXCODE == "UYAG1701CS"|EXCODE == "UYAG1801CS"|EXCODE == "UYAG1901CS") %>%
  mutate(MHWAMS=mean(HWAMS), MHWAMM=mean(HWAMM))
IBADAN <- FILES %>%
  filter(EXCODE == "UYIB1801CS"|EXCODE == "UYIB1901CS") %>%
  mutate(MHWAMS=mean(HWAMS), MHWAMM=mean(HWAMM))
IKENNE <- FILES %>%
  filter(EXCODE == "UYIK1701CS"|EXCODE == "UYIK1801CS"|EXCODE == "UYIK1901CS") %>%
  mutate(MHWAMS=mean(HWAMS), MHWAMM=mean(HWAMM))
MOKWA <- FILES %>%
  filter(EXCODE == "UYMO1701CS"|EXCODE == "UYMO1801CS"|EXCODE == "UYMO1901CS") %>%
  mutate(MHWAMS=mean(HWAMS), MHWAMM=mean(HWAMM))
ONNE <- FILES %>%
  filter(EXCODE == "UYON1801CS") %>%
  mutate(MHWAMS=mean(HWAMS), MHWAMM=mean(HWAMM))
OTOBI <- FILES %>%
  filter(EXCODE== "UYOT1801CS") %>%
  mutate(MHWAMS=mean(HWAMS), MHWAMM=mean(HWAMM))
UMUDIKE <- FILES %>%
  filter(EXCODE == "UYUM1801CS") %>%
  mutate(MHWAMS=mean(HWAMS), MHWAMM=mean(HWAMM))
ZARIA <- FILES %>%
  filter(EXCODE == "UYZA1901CS") %>%
  mutate(MHWAMS=mean(HWAMS), MHWAMM=mean(HWAMM))
UBIAJA <- FILES %>%
  filter(EXCODE == "UYUB1901CS") %>%
  mutate(MHWAMS=mean(HWAMS), MHWAMM=mean(HWAMM))


ggplot(FILES)  +
  geom_point(aes(x = observed, y = simulated, color = Location)) + 
  scale_color_manual("Location",
                     values = c("red", "#FFDD45", rgb(0.1,0.2,0.6),
                                "darkgreen", "grey80", "cyan",
                                "blue1", "burlywood1", "aquamarine",
                                "chocolate", "orange")) +
  geom_smooth(aes(x = observed, y = simulated), method = "lm")

             