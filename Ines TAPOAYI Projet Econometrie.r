### --- PACKAGES ---
library(devtools)
library(fredr)
library(zoo)
library(purrr)
library(tidyr)  
library(taceconomics)
library(dplyr)

### --- CLÉS API ---
taceconomics.apikey("sk_vi3tyYZnKTionSr9hAhXp2yxsPBbKSzSOq6TAZ87ykk")
fredr_set_key("e0f5ca87e7c47fdbacf6aee016397990")

### --- IMPORTATION DES DONNEES ---

# --- Taux directeur (FEDFUNDS)
taux <- getdata("FRED/DFF/USA")
taux <- data.frame(Date = index(taux), FEDFUNDS = as.numeric(taux)) %>%
  mutate(Trimestre = as.yearqtr(Date)) %>%
  group_by(Trimestre) %>%
  summarise(FEDFUNDS = mean(FEDFUNDS, na.rm = TRUE))

# --- PIB réel (GDPC1)
pib <- getdata("FRED/GDPC1/USA")
pib <- data.frame(Date = index(pib), PIB = as.numeric(pib)) %>%
  mutate(Trimestre = as.yearqtr(Date)) %>%
  dplyr::select(Trimestre, PIB)

# --- Taux de défaut bancaire (DRALACBN)
defaut <- getdata("FRED/DRALACBN/USA")
defaut <- data.frame(Date = index(defaut), DefaultRate = as.numeric(defaut)) %>%
  mutate(Trimestre = as.yearqtr(Date)) %>%
  dplyr::select(Trimestre, DefaultRate)

# --- Crédit bancaire total (TOTLL)
credit <- fredr(series_id = "TOTLL") %>%
  dplyr::select(date, value) %>%
  rename(Credit = value) %>%
  mutate(Trimestre = as.yearqtr(date)) %>%
  group_by(Trimestre) %>%
  summarise(Credit = mean(Credit, na.rm = TRUE))

### --- FUSION DES DONNEES ---
df <- list(taux, pib, credit, defaut) %>%
  reduce(full_join, by = "Trimestre") %>%
  arrange(Trimestre)

### --- CHOIX DE LA PÉRIODE 2000–2019 ---
df_final <- df %>%
  filter(Trimestre >= as.yearqtr("2000 Q1"),
         Trimestre <= as.yearqtr("2019 Q4"))

### --- CONVERSION EN MILLIERS POUR LE PIB & CREDIT ---
df_final <- df_final %>%
  mutate(
    PIB = PIB / 1000,
    Credit = Credit / 1000
  )

### --- TABLEAU STATISTIQUES ---
tab_stats <- df_final %>%
  summarise(
    across(
      c(FEDFUNDS, PIB, Credit, DefaultRate),
      list(
        Mean = ~mean(.x, na.rm = TRUE),
        SD   = ~sd(.x, na.rm = TRUE),
        Min  = ~min(.x, na.rm = TRUE),
        Max  = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

### --- AFFICHAGE DES STATISTIQUES
tab_stats
tab_stats <- tab_stats %>%
  mutate(across(where(is.numeric), ~format(.x, nsmall = 2)))


### --- GRAPHIQUE TAUX DIRECTEUR  ---
library(ggplot2)

df_final$Trimestre <- as.Date(df_final$Trimestre)

# Dates clés
dates_cle <- as.Date(c("2000-01-01", "2004-01-01", "2008-01-01",
                       "2012-01-01", "2016-01-01", "2019-01-01"))

p_fed <- ggplot(df_final, aes(x = Trimestre, y = FEDFUNDS)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  labs(
    title = "Taux directeur de la Fed aux USA (2000 - 2019)",
    x = "Année",
    y = "Taux directeur (%)"
  ) +
  scale_x_date(
    breaks = dates_cle,
    labels = format(dates_cle, "%Y"),
    limits = as.Date(c("2000-01-01", "2019-12-31"))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9)
  )

ggsave("Graphique_FEDFUNDS.png", plot = p_fed, width = 6, height = 4, dpi = 300)


### --- GRAPHIQUE VOLUME DE CRÉDIT BANCAIRE ---
library(ggplot2)

df_final$Trimestre <- as.Date(df_final$Trimestre)

# Dates clés
dates_cle <- as.Date(c("2000-01-01", "2004-01-01", "2008-01-01",
                       "2012-01-01", "2016-01-01", "2019-01-01"))

p_credit <- ggplot(df_final, aes(x = Trimestre, y = Credit)) +
  geom_line(color = "#E69F00", linewidth = 1.2) +  
  labs(
    title = "Volume total de crédit bancaire aux USA (2000 - 2019)",
    x = "Année",
    y = "Volume du crédit (milliards USD)"
  ) +
  scale_x_date(
    breaks = dates_cle,
    labels = format(dates_cle, "%Y"),
    limits = as.Date(c("2000-01-01", "2019-12-31"))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9)
  )

ggsave("Graphique_CREDIT.png", plot = p_credit, width = 6, height = 4, dpi = 300)


### --- GRAPHIQUE PIB  ---
library(ggplot2)

df_final$Trimestre <- as.Date(df_final$Trimestre)

# Dates clés
dates_cle <- as.Date(c("2000-01-01", "2004-01-01", "2008-01-01",
                       "2012-01-01", "2016-01-01", "2019-01-01"))

p_pib <- ggplot(df_final, aes(x = Trimestre, y = PIB)) +
  geom_line(color = "#009E73", linewidth = 1.2) + 
  labs(
    title = "PIB réel aux USA (2000 - 2019)",
    x = "Année",
    y = "PIB réel (milliards de dollars chaînés 2017)"
  ) +
  scale_x_date(
    breaks = dates_cle,
    labels = format(dates_cle, "%Y"),
    limits = as.Date(c("2000-01-01", "2019-12-31"))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9)
  )

ggsave("Graphique_PIB.png", plot = p_pib, width = 6, height = 4, dpi = 300)


### --- GRAPHIQUE TAUX DE DÉFAUT ---
library(ggplot2)

df_final$Trimestre <- as.Date(df_final$Trimestre)

# Dates clés
dates_cle <- as.Date(c("2000-01-01", "2004-01-01", "2008-01-01",
                       "2012-01-01", "2016-01-01", "2019-01-01"))

p_default <- ggplot(df_final, aes(x = Trimestre, y = DefaultRate)) +
  geom_line(color = "#D55E00", linewidth = 1.2) +  
  labs(
    title = "Taux de défaut bancaire aux USA (2000 - 2019)",
    x = "Année",
    y = "Taux de défaut (%)"
  ) +
  scale_x_date(
    breaks = dates_cle,
    labels = format(dates_cle, "%Y"),
    limits = as.Date(c("2000-01-01", "2019-12-31"))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9)
  )

ggsave("Graphique_DEFAULT.png", plot = p_default, width = 6, height = 4, dpi = 300)


### --- GRAPHIQUE TAUX VS CREDIT ---
library(dplyr)
library(ggplot2)

df_plot <- df_final %>%
  mutate(
    Date = as.Date(Trimestre),
    FEDFUNDS_std = as.numeric(scale(FEDFUNDS)),
    Credit_std   = as.numeric(scale(Credit))
  )

dates_cle <- as.Date(c("2000-01-01", "2004-01-01", "2008-01-01",
                       "2012-01-01", "2016-01-01", "2019-01-01"))

p_creditfed <- ggplot(df_plot, aes(x = Date)) +
  geom_line(aes(y = FEDFUNDS_std, color = "Taux directeur (FedFunds)"), linewidth = 1.3) +
  geom_line(aes(y = Credit_std, color = "Volume de crédit"), linewidth = 1.3, linetype = "dashed") +
  scale_color_manual(values = c(
    "Taux directeur (FedFunds)" = "#0072B2",
    "Volume de crédit" = "#E69F00"
  )) +
  labs(
    title = "Taux directeur et volume de crédit aux USA (2000 - 2019)",
    x = "Année",
    y = "Valeurs standardisées (z-scores)",
    color = ""
  ) +
  scale_x_date(
    breaks = dates_cle,
    labels = format(dates_cle, "%Y"),
    limits = as.Date(c("2000-01-01", "2019-12-31"))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9)
  )

ggsave("Graphique_Taux_VS_CREDIT.png",
       plot = p_creditfed,
       width = 6,
       height = 4,
       dpi = 300)


### --- TESTS DE STATIONARITÉ ---
library(urca)
library(tseries)
library(dplyr)

# Tests ADF
adf_fed <- ur.df(df_final$FEDFUNDS, type = "trend", selectlags = "AIC")
adf_pib <- ur.df(df_final$PIB, type = "trend", selectlags = "AIC")
adf_credit <- ur.df(df_final$Credit, type = "trend", selectlags = "AIC")
adf_defaut <- ur.df(df_final$DefaultRate, type = "trend", selectlags = "AIC")

# Tests KPSS
kpss_fed <- kpss.test(df_final$FEDFUNDS, null = "Trend")
kpss_pib <- kpss.test(df_final$PIB, null = "Trend")
kpss_credit <- kpss.test(df_final$Credit, null = "Trend")
kpss_defaut <- kpss.test(df_final$DefaultRate, null = "Trend")

# Tableau récapitulatif
tab_station <- data.frame(
  Variable = c("FEDFUNDS", "PIB", "Credit", "DefaultRate"),
  ADF_Statistique = round(c(adf_fed@teststat[1],
                            adf_pib@teststat[1],
                            adf_credit@teststat[1],
                            adf_defaut@teststat[1]), 3),
  KPSS_pvalue = round(c(kpss_fed$p.value,
                        kpss_pib$p.value,
                        kpss_credit$p.value,
                        kpss_defaut$p.value), 3)
)

print(tab_station)


# Rendre stationnaire les series non stationnaires

library(dplyr)
library(urca)
library(tseries)

df_step <- df_final %>%
  arrange(Trimestre) %>%
  mutate(
    lnPIB      = log(PIB),

    dFED       = FEDFUNDS - lag(FEDFUNDS, 1),
    d2FED      = dFED - lag(dFED, 1),

    dDefault   = DefaultRate - lag(DefaultRate, 1),
    d2Default  = dDefault - lag(dDefault, 1),

    dCredit    = Credit - lag(Credit, 1),

    dlnPIB     = lnPIB - lag(lnPIB, 1)
  ) %>%
  dplyr::select(Trimestre, d2FED, dCredit, d2Default, dlnPIB) %>%   
  tidyr::drop_na()

# Tests stationnarité
adf_d2fed     <- ur.df(df_step$d2FED, type = "drift", selectlags = "AIC")
adf_dcredit   <- ur.df(df_step$dCredit, type = "drift", selectlags = "AIC")
adf_d2default <- ur.df(df_step$d2Default, type = "drift", selectlags = "AIC")
adf_dlnpib    <- ur.df(df_step$dlnPIB, type = "drift", selectlags = "AIC")

kpss_d2fed     <- kpss.test(df_step$d2FED, null = "Level")
kpss_dcredit   <- kpss.test(df_step$dCredit, null = "Level")
kpss_d2default <- kpss.test(df_step$d2Default, null = "Level")
kpss_dlnpib    <- kpss.test(df_step$dlnPIB, null = "Level")

tab_station_step <- data.frame(
  Variable = c("Δ²FEDFUNDS","ΔCredit","Δ²DefaultRate","Δlog(PIB)"),
  ADF_Statistique = round(c(adf_d2fed@teststat[1],
                            adf_dcredit@teststat[1],
                            adf_d2default@teststat[1],
                            adf_dlnpib@teststat[1]), 3),
  KPSS_pvalue = round(c(kpss_d2fed$p.value,
                        kpss_dcredit$p.value,
                        kpss_d2default$p.value,
                        kpss_dlnpib$p.value), 3)
)

print(tab_station_step)


# Création des séries stationnaires transformées
df_final <- df_final %>%
  mutate(
    dlogPIB = c(NA, diff(log(PIB), lag = 1)),              # Δlog(PIB)
    dCredit = c(NA, diff(Credit, lag = 1)),                # ΔCredit
    d2DefaultRate = c(NA, NA, diff(DefaultRate, differences = 2)), # Δ²DefaultRate
    d2FEDFUNDS = c(NA, NA, diff(FEDFUNDS, differences = 2))        # Δ²FEDFUNDS
  )

# Graphique séries stationnarisées
library(ggplot2)
library(gridExtra)

# --- Séries en niveau ---
g1 <- ggplot(df_final, aes(x = Trimestre, y = PIB)) +
  geom_line() +
  labs(title = "PIB (niveau)") +
  theme_minimal()

g2 <- ggplot(df_final, aes(x = Trimestre, y = Credit)) +
  geom_line() +
  labs(title = "Crédit bancaire (niveau)") +
  theme_minimal()

g3 <- ggplot(df_final, aes(x = Trimestre, y = DefaultRate)) +
  geom_line() +
  labs(title = "Taux de défaut (niveau)") +
  theme_minimal()

g4 <- ggplot(df_final, aes(x = Trimestre, y = FEDFUNDS)) +
  geom_line() +
  labs(title = "Taux directeur FED (niveau)") +
  theme_minimal()

# --- Séries stationnaires ---
h1 <- ggplot(df_final, aes(x = Trimestre, y = dlogPIB)) +
  geom_line() +
  labs(title = "Δlog(PIB)") +
  theme_minimal()

h2 <- ggplot(df_final, aes(x = Trimestre, y = dCredit)) +
  geom_line() +
  labs(title = "ΔCredit") +
  theme_minimal()

h3 <- ggplot(df_final, aes(x = Trimestre, y = d2DefaultRate)) +
  geom_line() +
  labs(title = "Δ²DefaultRate") +
  theme_minimal()

h4 <- ggplot(df_final, aes(x = Trimestre, y = d2FEDFUNDS)) +
  geom_line() +
  labs(title = "Δ²FEDFUNDS") +
  theme_minimal()

# --- Figure finale (4 lignes x 2 colonnes) ---
library(ggplot2)
library(gridExtra)

# Figure multi-panels
p <- grid.arrange(g1, h1,
                  g2, h2,
                  g3, h3,
                  g4, h4,
                  ncol = 2)

# Export en PNG (haute résolution)
ggsave("stationnarite_series.png", p,
       width = 10, height = 12, dpi = 300)













### --- CHOIX DU NOMBRE DE RETARDS (p) --- 
library(dplyr)
library(vars)
df_var <- df_final %>% 
dplyr::select(dlogPIB, dCredit, d2DefaultRate, d2FEDFUNDS) %>% 
na.omit() 

lag_selection <- VARselect(df_var, lag.max = 8, type = "const") 

lag_selection


### --- ESTIMATION DU VAR (1) ---
var_model <- VAR(df_var, p = 1, type = "const")
summary(var_model)

# Tests de diagnostic du modèle

library(vars)
library(lmtest)
library(tseries)

var_model <- VAR(df_var, p = 1, type = "const")

# Autocorrélation des résidus
serial.test(var_model, type = "PT.asymptotic")

# Hétéroscédasticité (ARCH)
arch.test(var_model)

# Normalité multivariée
normality.test(var_model)

# Stabilité 
roots(var_model, modulus = TRUE)


### --- CAUSALITE DE GRANGER ---
# Causalité : FedFunds → Crédit
causality(var_model, cause = "d2FEDFUNDS")

# Causalité : Crédit → FedFunds
causality(var_model, cause = "dCredit")

# Causalité : FedFunds → PIB
causality(var_model, cause = "d2FEDFUNDS")

# Causalité : PIB → FedFunds
causality(var_model, cause = "dlogPIB")

# Causalité : Crédit → Défauts
causality(var_model, cause = "dCredit")

# Causalité : Défauts → Crédit
causality(var_model, cause = "d2DefaultRate")

# Causalité : dCredit → d2DefaultRate
causality(var_model, cause = "dCredit")$Granger



### --- ESTIMATION DU SVAR ---
install.packages("svars")
library(vars)
library(svars)

# On repart de VAR (1)
var_model <- VAR(df_var, p = 1, type = "const")

# Identification structurelle par décomposition de Cholesky
svar_model <- id.chol(var_model, order = c("d2FEDFUNDS", "dCredit", "dlogPIB", "d2DefaultRate"))

summary(svar_model)

### --- IRF ---
library(ggplot2)

while (dev.cur() > 1) dev.off()

# IRF
irf_model <- irf(svar_model,
                 impulse = "d2FEDFUNDS",
                 response = c("dCredit", "dlogPIB", "d2DefaultRate"),
                 n.ahead = 12,
                 boot = TRUE,
                 ci = 0.95)

png("IRF.png", width = 1800, height = 900, res = 200)
print(plot(irf_model, main = "Réponses à un choc de politique monétaire"))
dev.off()

### --- FEVD ---
# Décomposition de la variance des erreurs de prévision
fevd_model <- fevd(var_model, n.ahead = 10)

fevd_model

# Fermer tous les devices ouverts avant export
while (dev.cur() > 1) dev.off()

png("FEVD.png", width = 1800, height = 1200, res = 200)
print(plot(fevd_model))
dev.off()


### --- PRÉVISION EN NIVEAU (taux de défaut reconstruit) ---
last_default <- tail(df_final$DefaultRate, 1)

# Variation sans choc
cum_baseline <- cumsum(baseline)

# Variation avec choc
cum_shock <- cumsum(shock)

# Reconstruction niveau
default_baseline <- last_default + cum_baseline
default_shock <- last_default + cum_shock

df_forecast_level <- data.frame(
  horizon = 1:12,
  baseline = default_baseline,
  shock = default_shock
)

p_level <- ggplot(df_forecast_level, aes(x = horizon)) +
  geom_line(aes(y = baseline, color = "Taux de défaut - scénario baseline"), linewidth = 1.6) +
  geom_line(aes(y = shock, color = "Taux de défaut - resserrement Fed (+1σ)"),
            linewidth = 1.6, linetype = "dashed") +
  labs(title = "Prévision du taux de défaut bancaire (niveau) sur 12 trimestres",
       x = "Horizon (trimestres)",
       y = "Taux de défaut (%)") +
  scale_color_manual(values = c(
    "Taux de défaut - scénario baseline" = "#0072B2",
    "Taux de défaut - resserrement Fed (+1σ)" = "#D55E00"
  )) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

p_level

ggsave("Previsions_DEFAULT_NIVEAU.png", plot = p_level, width = 7, height = 4, dpi = 300)