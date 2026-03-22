#### Funcões para aula de competição ####

# Modelo de lotka volterra
comp_lv_logic <- function(tempo, estado, parametros) {
  with(as.list(c(estado, parametros)), {
    dN1 <- r1 * N1 * ((K1 - N1 - alpha * N2) / K1)
    dN2 <- r2 * N2 * ((K2 - N2 - beta * N1) / K2)
    return(list(c(dN1, dN2)))
  })
}

# Função para simular competição
simular_competicao <- function(r1, r2, N1_ini, N2_ini, alpha, beta, K1, K2,
                               t_max = 150, Descrever_resultado = TRUE) {

  params <- c(r1 = r1, r2 = r2, alpha = alpha, beta = beta, K1 = K1, K2 = K2)
  init <- c(N1 = N1_ini, N2 = N2_ini)
  time <- seq(0, t_max, by = 0.1)

  out <- as.data.frame(ode(y = init, times = time, func = comp_lv_logic,
                           parms = params))

  # --- Lógica de Diagnóstico ---
  t_exc_e1 <- out$time[which(out$N1 < 1)[1]]
  t_exc_e2 <- out$time[which(out$N2 < 1)[1]]
  p1 <- K1 > (K2 / beta)
  p2 <- K2 > (K1 / alpha)

  if (!p1 && !p2) {
    msg <- "✅ Coexistência Estável: Equilíbrio atingido!\nCompetição intraespecífica é maior que interespecífica."
  } else {
    vencedora <- ifelse(out$N1[nrow(out)] > out$N2[nrow(out)], "Espécie 1", "Espécie 2")
    t_venc <- if(vencedora == "Espécie 1") t_exc_e2 else t_exc_e1
    txt_tempo <- if(is.na(t_venc)) "(Exclusão em curso)" else paste0("(tempo: ", round(t_venc, 1), ")")

    if (p1 && p2) msg <- paste0("⚠️ Coexistência Instável!\nCompetição interespecífica mais forte que intraespecífica: ", vencedora, " venceu ", txt_tempo, ".")
    else if (!p1 && p2) msg <- paste0("❌ Exclusão Competitiva: Espécie 2 excluiu Espécie 1 ", txt_tempo, ".")
    else msg <- paste0("❌ Exclusão Competitiva: Espécie 1 excluiu Espécie 2 ", txt_tempo, ".")
  }

  if(Descrever_resultado) cat("\n--- ANÁLISE ---\n", msg, "\n---------------\n")

  # --- GRÁFICO 1: Série Temporal com Labels em K ---
  g_tempo <- ggplot(out, aes(x = time)) +
    geom_line(aes(y = N1, color = "Espécie 1"), size = 1.2) +
    geom_line(aes(y = N2, color = "Espécie 2"), size = 1.2) +
    # Linhas de K
    geom_hline(yintercept = K1, color = "blue", linetype = "dashed",
               alpha = 0.4) +
    geom_hline(yintercept = K2, color = "red", linetype = "dashed",
               alpha = 0.4) +
    # Texto de K na extremidade direita
    annotate("text", x = t_max * 0.95, y = K1, label = paste0("K1 (", K1, ")"),
             color = "blue", vjust = -0.5, fontface = "bold", size = 2.2) +
    annotate("text", x = t_max * 0.95, y = K2, label = paste0("K2 (", K2, ")"),
             color = "red", vjust = -0.5, fontface = "bold", size = 2.2) +
    scale_color_manual(values = c("Espécie 1" = "blue", "Espécie 2" = "red")) +
    labs(title = "Crescimento Populacional", x = "Tempo", y = "População (N)",
         color = "Espécie") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # --- GRÁFICO 2: Isóclinas com Valores nos Eixos ---
  max_x <- max(K1, K2/beta) * 1.1
  max_y <- max(K2, K1/alpha) * 1.1

  grid_data <- expand.grid(N1 = seq(0.1, max_x, length.out = 10),
                           N2 = seq(0.1, max_y, length.out = 10))
  grid_data$dir1 <- with(grid_data, ifelse(K1 - N1 - alpha * N2 > 0, 1, -1))
  grid_data$dir2 <- with(grid_data, ifelse(K2 - N2 - beta * N1 > 0, 1, -1))

  lab_x <- c(paste0("K1\n(", K1, ")"), paste0("K2/β\n(", round(K2/beta, 1), ")"))
  lab_y <- c(paste0("K2 (", K2, ")"), paste0("K1/α (", round(K1/alpha, 1), ")"))

  g_isoclinas <- ggplot(out, aes(x = N1, y = N2)) +
    geom_segment(data = grid_data,
                 aes(x = N1, y = N2, xend = N1 + dir1 * (max_x*0.04), yend = N2),
                 arrow = arrow(length = unit(0.1, "cm")),
                 color = "blue", alpha = 0.15) +
    geom_segment(data = grid_data,
                 aes(x = N1, y = N2, xend = N1, yend = N2 + dir2 * (max_y*0.04)),
                 arrow = arrow(length = unit(0.1, "cm")),
                 color = "red", alpha = 0.15) +
    annotate("segment", x = K1, y = 0, xend = 0, yend = K1/alpha,
             color = "blue", size = 1.3) +
    annotate("segment", x = K2/beta, y = 0, xend = 0, yend = K2,
             color = "red", size = 1.3) +
    geom_path(size = 1, color = "black",
              arrow = arrow(length = unit(0.25, "cm"))) +
    scale_x_continuous(breaks = c(K1, K2/beta), labels = lab_x) +
    scale_y_continuous(breaks = c(K2, K1/alpha), labels = lab_y) +
    labs(title = "Isóclinas de crescimento zero", x = "N1", y = "N2") +
    theme_minimal() + coord_cartesian(xlim = c(0, max_x), ylim = c(0, max_y))

  grid.arrange(g_tempo, g_isoclinas, ncol = 2)
}



# # --- Exemplos de Execução ---
#
# # Exclusão da Espécie 1
# s1 <- simular_competicao(r1=0.2, r2=0.2, N1_ini=40, N2_ini=10, alpha=1,
#                          beta=0.5,
#                          K1=80, K2=120)
# ggsave("Imagens/exclusao_1.png", s1, dpi = 600, width = 9, height = 4)
#
# # Exclusão da Espécie 2
# s2 <- simular_competicao(r1=0.2, r2=0.2, N1_ini=20, N2_ini=80, alpha=0.8,
#                          beta=1,
#                          K1=110, K2=80)
# ggsave("Imagens/exclusao_2.png", s2, dpi = 600, width = 9, height = 4)
#
# # Coexistência Estável
# s3 <- simular_competicao(r1=0.3, r2=0.3, N1_ini=10, N2_ini=5, alpha=0.5,
#                          beta=0.5,
#                          K1=100, K2=120)
# ggsave("Imagens/equilibrio_estavel.png", s3, dpi = 600, width = 9, height = 4)
#
# # Coexistência Instavel
# s4 <- simular_competicao(r1=0.1, r2=0.5, N1_ini=60, N2_ini=45, alpha=1.5,
#                          beta=1.5,
#                          K1=100, K2=110)
# ggsave("Imagens/equilibrio_instavel.png", s4, dpi = 600, width = 9, height = 4)
#
# s5 <- simular_competicao(r1=0.1, r2=0.5, N1_ini=45, N2_ini=60, alpha=1.5,
#                          beta=1.5,
#                          K1=100, K2=110)
# ggsave("Imagens/equilibrio_instavel_2.png", s5, dpi = 600, width = 9,
#        height = 4)


simular_competicao_disturbio <- function(r1, r2, N1_ini, N2_ini, alpha,
                                         beta, K1, K2,
                                         t_max = 150,
                                         tipo_disturbio = "densidade",
                                         frequencia_t = 20,
                                         gatilho_k = 0.7,
                                         intensidade = 0.5) {

  # 1. Configuração da Simulação (Método de Euler)
  dt <- 0.1
  passos <- t_max / dt
  res <- data.frame(time = seq(0, t_max, by = dt),
                    N1 = numeric(passos + 1),
                    N2 = numeric(passos + 1),
                    evento = FALSE)

  curr_N1 <- N1_ini
  curr_N2 <- N2_ini
  res[1, c("N1", "N2")] <- c(curr_N1, curr_N2)

  for (i in 2:(passos + 1)) {
    t_at <- res$time[i]

    # Crescimento Lotka-Volterra
    dN1 <- r1 * curr_N1 * ((K1 - curr_N1 - alpha * curr_N2) / K1) * dt
    dN2 <- r2 * curr_N2 * ((K2 - curr_N2 - beta * curr_N1) / K2) * dt

    curr_N1 <- max(0, curr_N1 + dN1)
    curr_N2 <- max(0, curr_N2 + dN2)

    # Lógica de Distúrbio no competidor mais forte
    dist <- FALSE
    venc <- if(curr_N1 > curr_N2) "E1" else "E2"

    if (tipo_disturbio == "tempo") {
      if (round(t_at, 1) %% frequencia_t == 0) dist <- TRUE
    } else {
      if (venc == "E1" && curr_N1 > (K1 * gatilho_k)) dist <- TRUE
      if (venc == "E2" && curr_N2 > (K2 * gatilho_k)) dist <- TRUE
    }

    if (dist) {
      if (venc == "E1") curr_N1 <- curr_N1 * (1 - intensidade)
      if (venc == "E2") curr_N2 <- curr_N2 * (1 - intensidade)
      res$evento[i] <- TRUE
    }
    res[i, c("N1", "N2")] <- c(curr_N1, curr_N2)
  }

  # --- GRÁFICO 1: Série Temporal com K ---
  g_tempo <- ggplot(res, aes(x = time)) +
    geom_line(aes(y = N1, color = "Espécie 1"), size = 1.1) +
    geom_line(aes(y = N2, color = "Espécie 2"), size = 1.1) +
    # Linhas de K e Rótulos
    geom_hline(yintercept = K1, color = "blue", linetype = "dashed",
               alpha = 0.4) +
    geom_hline(yintercept = K2, color = "red", linetype = "dashed",
               alpha = 0.4) +
    annotate("text", x = t_max * 0.95, y = K1, label = paste0("K1 (", K1, ")"),
             color = "blue", vjust = -0.5, fontface = "bold") +
    annotate("text", x = t_max * 0.95, y = K2, label = paste0("K2 (", K2, ")"),
             color = "red", vjust = -0.5, fontface = "bold") +
    # Marcas de Distúrbio
    geom_vline(data = res[res$evento, ],
               aes(xintercept = time), linetype = "dotted",
               color = "gray30", alpha = 0.5) +
    scale_color_manual(values = c("Espécie 1" = "blue", "Espécie 2" = "red")) +
    labs(title = "Série Temporal com Eventos de Distúrbio",
         subtitle = "Linhas pontilhadas indicam eventos de disturbio no competidor mais forte",
         x = "Tempo", y = "N",
         color = "Legenda") +
    theme_minimal()

  # --- GRÁFICO 2: Isóclinas com Valores ---
  max_x <- max(K1, K2/beta, res$N1) * 1.1
  max_y <- max(K2, K1/alpha, res$N2) * 1.1

  lab_x <- c(paste0("K1\n(", K1, ")"),
             paste0("K2/β\n(", round(K2/beta, 1), ")"))
  lab_y <- c(paste0("K2 (", K2, ")"),
             paste0("K1/α (", round(K1/alpha, 1), ")"))

  g_isoclinas <- ggplot(res, aes(x = N1, y = N2)) +
    annotate("segment", x = K1, y = 0, xend = 0, yend = K1/alpha,
             color = "blue", size = 1.3, alpha = 0.6) +
    annotate("segment", x = K2/beta, y = 0, xend = 0,
             yend = K2, color = "red", size = 1.3, alpha = 0.6) +
    geom_path(size = 0.8, color = "black",
              arrow = arrow(length = unit(0.2, "cm"))) +
    scale_x_continuous(breaks = c(K1, K2/beta), labels = lab_x) +
    scale_y_continuous(breaks = c(K2, K1/alpha), labels = lab_y) +
    labs(title = "Trajetória e Equilíbrio Dinâmico", x = "N1", y = "N2",
         subtitle = "O distúrbio 'empurra' a trajetória para trás") +
    theme_minimal() + coord_cartesian(xlim = c(0, max_x), ylim = c(0, max_y))

  grid.arrange(g_tempo, g_isoclinas, ncol = 2)
}

# # --- TESTE DO CENÁRIO DE COEXISTÊNCIA ---
# # Com disturbio dependente de densidade (ex: predação, herbivoria e doenças)
# d <- simular_competicao_disturbio(r1=0.5, r2=0.3, N1_ini=10, N2_ini=10, alpha=1.5,
#                                   beta=0.5, K1=100, K2=80,
#                                   tipo_disturbio = "densidade",
#                                   gatilho_k = 0.8,
#                                   intensidade = 0.4)
# ggsave("Imagens/disturbio_densidade.png", d, dpi = 600, width = 10, height = 4,
#        scale = 1.25)
#
# # Com disturbio ao longo do tempo
# ds <- simular_competicao_disturbio(r1=0.5, r2=0.3, N1_ini=10, N2_ini=10, alpha=1.5,
#                                    beta=0.5, K1=100, K2=80,
#                                    tipo_disturbio = "tempo", frequencia_t = 20,
#                                    intensidade = 0.75)
# ggsave("Imagens/disturbio_tempo.png", ds, dpi = 600, width = 10, height = 4,
#        scale = 1.25)
