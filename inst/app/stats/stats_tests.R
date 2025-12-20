# Funciones de pruebas estadísticas -------------------------------------------

do_anova <- function(df, post_hoc = "Tukey", control_group = NULL) {
  aovm <- aov(Valor ~ Label, data = df)
  switch(post_hoc,
         "Tukey"      = broom::tidy(TukeyHSD(aovm)) |> dplyr::rename(p.adj = adj.p.value),
         "Bonferroni" = rstatix::pairwise_t_test(df, Valor ~ Label, p.adjust.method = "bonferroni"),
         "Sidak"      = rstatix::pairwise_t_test(df, Valor ~ Label, p.adjust.method = "sidak"),
         "Dunnett"    = dunnett_to_tibble(
           DescTools::DunnettTest(Valor ~ Label, data = set_control(df, control_group))),
         "Scheffe"    = pmcmr_to_tibble(PMCMRplus::scheffeTest(aovm, "Label")),
         "GamesHowell"= rstatix::games_howell_test(df, Valor ~ Label)
  )
}

do_kw <- function(df, post_hoc = "Dunn") {
  switch(post_hoc,
         "Dunn"    = rstatix::dunn_test(df, Valor ~ Label, p.adjust.method = "bonferroni"),
         "Conover" = pmcmr_to_tibble(PMCMRplus::kwAllPairsConoverTest(df$Valor, df$Label)),
         "Nemenyi" = pmcmr_to_tibble(PMCMRplus::kwAllPairsNemenyiTest(df$Valor, df$Label)),
         "DSCF"    = {
           f <- if (exists("kwAllPairsDSCFTest", asNamespace("PMCMRplus"), FALSE))
             PMCMRplus::kwAllPairsDSCFTest else PMCMRplus::kwAllPairsDscfTest
           pmcmr_to_tibble(f(df$Valor, df$Label))
         }
  )
}
