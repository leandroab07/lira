library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyWidgets)
library(bibliometrix)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(DT)
library(waiter)

clean_str <- function(x, n = 100) {
  substr(gsub(";", " | ", as.character(x)), 1, n)
}

get_country_col <- function(df) {
  # AU_CO = país de todos os autores; AU1_CO = país do autor de correspondência.
  # C1 (afiliação completa) é deliberadamente excluído: usá-lo como país produz
  # contagens incorretas (a string inteira da afiliação seria tratada como país).
  intersect(c("AU_CO", "AU1_CO"), names(df))[1]
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

# grepl que nunca interrompe o pipeline: padrões inválidos (digitados pelo usuário
# nos tiers de revistas) retornam FALSE em vez de gerar erro.
safe_grepl <- function(pattern, x) {
  tryCatch(grepl(pattern, x), error = function(e) rep(FALSE, length(x)))
}

extract_search_groups <- function(query) {
  if (is.null(query) || !nzchar(trimws(query))) return(list())

  q <- toupper(query)
  q <- gsub("\\n", " ", q)
  q <- gsub("\\s+", " ", q)
  q <- gsub("\\s+AND\\s+", " && ", q)
  q <- gsub("\\s+NOT\\s+", " ##NOT## ", q)

  groups <- stringr::str_extract_all(q, "\\([^\\(\\)]+\\)")[[1]]
  out <- list()

  if (length(groups) > 0) {
    for (g in groups) {
      g2 <- gsub("^\\(|\\)$", "", g)
      g2 <- gsub("##NOT##.*$", "", g2)
      parts <- unlist(strsplit(g2, "\\s+OR\\s+"))
      parts <- gsub('"', "", parts)
      parts <- trimws(parts)
      parts <- parts[nchar(parts) > 1]
      if (length(parts) > 0) out[[length(out) + 1]] <- unique(parts)
    }
  } else {
    parts <- unlist(strsplit(q, "&&"))
    parts <- gsub("##NOT##.*$", "", parts)
    parts <- gsub('"', "", parts)
    parts <- trimws(parts)
    parts <- parts[nchar(parts) > 1]
    if (length(parts) > 0) {
      for (p in parts) out[[length(out) + 1]] <- unique(p)
    }
  }

  out
}

build_criteria_from_search <- function(query) {
  groups <- extract_search_groups(query)

  if (length(groups) == 0) {
    return(data.frame(
      id       = character(0),
      nome     = character(0),
      peso     = integer(0),
      keywords = character(0),
      stringsAsFactors = FALSE
    ))
  }

  make_name <- function(x, idx) {
    first_term <- trimws(x[1])
    first_term <- gsub("[^A-Z0-9 -]", "", first_term)
    first_term <- stringr::str_to_title(first_term)
    if (!nzchar(first_term)) first_term <- paste("Critério", idx)
    paste0("Grupo ", idx, " — ", first_term)
  }

  pesos_base <- c(5, 4, 3, 3, 2, 2, 2, 1, 1, 1)
  pesos <- pesos_base[seq_len(min(length(groups), length(pesos_base)))]
  if (length(groups) > length(pesos_base)) {
    pesos <- c(pesos, rep(1, length(groups) - length(pesos_base)))
  }

  data.frame(
    id       = paste0("T", seq_along(groups)),
    nome     = vapply(seq_along(groups), function(i) make_name(groups[[i]], i), character(1)),
    peso     = pesos,
    keywords = vapply(groups, function(x) paste(unique(x), collapse = "; "), character(1)),
    stringsAsFactors = FALSE
  )
}

score_citations_raw <- function(tc_num) {
  dplyr::case_when(
    tc_num >= 200 ~ 10,
    tc_num >= 100 ~ 8,
    tc_num >= 50  ~ 6,
    tc_num >= 20  ~ 4,
    tc_num >= 10  ~ 2,
    tc_num >= 1   ~ 1,
    TRUE          ~ 0
  )
}

score_citations_time_adjusted <- function(tc_per_year) {
  dplyr::case_when(
    tc_per_year >= 20 ~ 10,
    tc_per_year >= 10 ~ 8,
    tc_per_year >= 5  ~ 6,
    tc_per_year >= 2  ~ 4,
    tc_per_year >= 1  ~ 2,
    tc_per_year > 0   ~ 1,
    TRUE              ~ 0
  )
}

score_recency_fun <- function(py_num, ref_year = as.integer(format(Sys.Date(), "%Y"))) {
  # Intervalos relativos ao ano de referência (data da busca):
  # 0-1 ano atrás → 5 | 2-3 → 4 | 4-5 → 3 | 6-7 → 2 | 8-9 → 1 | 10+ → 0
  dplyr::case_when(
    py_num >= ref_year - 1 ~ 5,
    py_num >= ref_year - 3 ~ 4,
    py_num >= ref_year - 5 ~ 3,
    py_num >= ref_year - 7 ~ 2,
    py_num >= ref_year - 9 ~ 1,
    TRUE ~ 0
  )
}

criteria_empty_df <- function() {
  data.frame(
    id       = character(0),
    nome     = character(0),
    peso     = integer(0),
    keywords = character(0),
    stringsAsFactors = FALSE
  )
}

thresholds_are_valid <- function(p1, p2, p3, removed) {
  isTRUE(!any(is.na(c(p1, p2, p3, removed)))) &&
    p1 > p2 && p2 > p3 && p3 > removed && removed >= 0
}

apply_priority_labels <- function(df, p1, p2, p3, removed) {
  if (is.null(df) || nrow(df) == 0) return(df)

  df %>%
    mutate(
      prioridade = case_when(
        score_final >= p1 ~ "P1 - OBRIGATORIA",
        score_final >= p2 ~ "P2 - RECOMENDADA",
        score_final >= p3 ~ "P3 - OPCIONAL",
        score_final < removed ~ "P4 - REMOVIDO",
        TRUE ~ "ABAIXO DO CORTE"
      )
    )
}

safe_theme_title <- function(x) {
  x <- if (length(x) == 0 || is.null(x) || is.na(x)) "" else as.character(x)
  x <- trimws(x)
  if (nchar(x) == 0) "Tema não definido" else x
}

calc_growth_rate <- function(x) {
  # AAGR: média aritmética das taxas de crescimento ano a ano.
  # Mais robusto que CAGR para séries oscilatórias ou com zeros.
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) < 2) return(NA_real_)
  denominators <- pmax(x[-length(x)], 1)
  rates <- diff(x) / denominators * 100
  rates <- rates[is.finite(rates)]
  if (length(rates) == 0) return(NA_real_)
  round(mean(rates), 2)
}

top_institutions_df <- function(df, top_n = 20) {
  inst_col <- intersect(c("AU_UN", "C1", "RP"), names(df))[1]
  if (length(inst_col) == 0 || is.na(inst_col)) {
    return(data.frame(INST = character(0), N = integer(0), stringsAsFactors = FALSE))
  }

  x <- as.character(df[[inst_col]])
  x[is.na(x)] <- ""

  tibble(raw = x) %>%
    filter(raw != "") %>%
    mutate(raw = strsplit(raw, ";")) %>%
    tidyr::unnest(raw) %>%
    mutate(
      raw = gsub("\\\\[[^]]*\\\\]", "", raw),
      raw = trimws(raw),
      INST = trimws(toupper(sub(",.*$", "", raw)))
    ) %>%
    filter(INST != "", nchar(INST) > 2, nchar(INST) < 80) %>%
    count(INST, name = "N", sort = TRUE) %>%
    head(top_n)
}

rescale_num <- function(x, to = c(1, 10)) {
  x <- as.numeric(x)
  if (length(x) == 0) return(numeric(0))
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(mean(to), length(x)))
  (x - rng[1]) / diff(rng) * diff(to) + to[1]
}

priority_dark <- function(x) {
  dplyr::case_when(
    x == "P1 - OBRIGATORIA" ~ "#1E40AF",
    x == "P2 - RECOMENDADA" ~ "#1E8449",
    x == "P3 - OPCIONAL" ~ "#C2410C",
    x == "P4 - REMOVIDO" ~ "#B91C1C",
    x == "ABAIXO DO CORTE" ~ "#E67E22",
    TRUE ~ "#5D6D7E"
  )
}

priority_light <- function(x) {
  dplyr::case_when(
    x == "P1 - OBRIGATORIA" ~ "#DCEAF8",
    x == "P2 - RECOMENDADA" ~ "#DFF2E5",
    x == "P3 - OPCIONAL" ~ "#FDEBD0",
    x == "P4 - REMOVIDO" ~ "#FADBD8",
    x == "ABAIXO DO CORTE" ~ "#FEF3E8",
    TRUE ~ "#F4F6F7"
  )
}

network_palette <- function(n) {
  base_cols <- c("#1E40AF", "#1E8449", "#C2410C", "#B91C1C", "#8E44AD", "#17A2B8", "#7F8C8D", "#6C5CE7")
  rep(base_cols, length.out = max(1, n))
}

build_network_bundle <- function(df, analysis = "co-occurrences", network = "keywords", top_n = 35, sep = ";", seed = 42L) {
  empty_metrics <- data.frame(
    Indicador = c("Nós", "Arestas", "Densidade", "Componentes", "Grau médio", "Modularidade", "Clusters"),
    Valor = c(0, 0, 0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )

  if (is.null(df) || nrow(df) == 0 || !requireNamespace("igraph", quietly = TRUE)) {
    return(list(graph = NULL, metrics = empty_metrics, nodes = data.frame()))
  }

  mat <- tryCatch(
    bibliometrix::biblioNetwork(df, analysis = analysis, network = network, sep = sep),
    error = function(e) NULL
  )

  if (is.null(mat) || length(mat) == 0) {
    return(list(graph = NULL, metrics = empty_metrics, nodes = data.frame()))
  }

  mat <- as.matrix(mat)
  if (nrow(mat) < 2 || ncol(mat) < 2) {
    return(list(graph = NULL, metrics = empty_metrics, nodes = data.frame()))
  }

  suppressWarnings(diag(mat) <- 0)
  mat[is.na(mat)] <- 0

  rs <- rowSums(mat, na.rm = TRUE)
  keep <- names(sort(rs, decreasing = TRUE))
  keep <- keep[rs[keep] > 0]
  keep <- head(keep, top_n)

  if (length(keep) < 3) {
    return(list(graph = NULL, metrics = empty_metrics, nodes = data.frame()))
  }

  mat2 <- mat[keep, keep, drop = FALSE]

  g <- tryCatch(
    igraph::graph_from_adjacency_matrix(mat2, mode = "undirected", weighted = TRUE, diag = FALSE),
    error = function(e) NULL
  )

  if (is.null(g)) {
    return(list(graph = NULL, metrics = empty_metrics, nodes = data.frame()))
  }

  g <- igraph::simplify(g, remove.loops = TRUE, edge.attr.comb = "sum")

  if (igraph::gorder(g) < 3) {
    return(list(graph = NULL, metrics = empty_metrics, nodes = data.frame()))
  }

  deg <- igraph::degree(g)
  g <- igraph::delete_vertices(g, deg == 0)

  if (igraph::gorder(g) < 3) {
    return(list(graph = NULL, metrics = empty_metrics, nodes = data.frame()))
  }

  cl <- tryCatch({
    set.seed(seed)
    igraph::cluster_louvain(g, weights = igraph::E(g)$weight)
  }, error = function(e) NULL)

  if (is.null(cl)) {
    igraph::V(g)$cluster <- 1L
    modularity_val <- 0
    n_clusters <- 1
  } else {
    igraph::V(g)$cluster <- as.integer(igraph::membership(cl))
    modularity_val <- round(igraph::modularity(cl), 3)
    n_clusters <- length(unique(igraph::membership(cl)))
  }

  igraph::V(g)$degree <- igraph::degree(g)
  igraph::V(g)$strength <- round(igraph::strength(g, weights = igraph::E(g)$weight), 1)

  nodes_df <- data.frame(
    No = igraph::V(g)$name,
    Cluster = igraph::V(g)$cluster,
    Grau = igraph::V(g)$degree,
    Forca = igraph::V(g)$strength,
    stringsAsFactors = FALSE
  ) %>%
    arrange(Cluster, desc(Forca), desc(Grau))

  metrics_df <- data.frame(
    Indicador = c("Nós", "Arestas", "Densidade", "Componentes", "Grau médio", "Modularidade", "Clusters"),
    Valor = c(
      igraph::gorder(g),
      igraph::gsize(g),
      round(igraph::edge_density(g), 3),
      igraph::components(g)$no,
      round(mean(igraph::degree(g)), 2),
      modularity_val,
      n_clusters
    ),
    stringsAsFactors = FALSE
  )

  list(graph = g, metrics = metrics_df, nodes = nodes_df)
}

cluster_terms_df <- function(net, top_terms = 6) {
  if (is.null(net$graph) || is.null(net$nodes) || nrow(net$nodes) == 0) {
    return(data.frame(Cluster = integer(0), Tamanho = integer(0), Forca_media = numeric(0), Principais_termos = character(0)))
  }

  net$nodes %>%
    group_by(Cluster) %>%
    arrange(desc(Forca), desc(Grau), .by_group = TRUE) %>%
    summarise(
      Tamanho = n(),
      Forca_media = round(mean(Forca, na.rm = TRUE), 2),
      Principais_termos = paste(head(No, top_terms), collapse = " | "),
      .groups = "drop"
    ) %>%
    arrange(Cluster)
}

network_summary_row <- function(net, nome) {
  if (is.null(net$graph) || nrow(net$metrics) == 0) {
    return(data.frame(
      Rede = nome, Nos = 0, Arestas = 0, Densidade = 0, Componentes = 0, Clusters = 0, stringsAsFactors = FALSE
    ))
  }
  tibble::tibble(
    Rede = nome,
    Nos = net$metrics$Valor[net$metrics$Indicador == "Nós"],
    Arestas = net$metrics$Valor[net$metrics$Indicador == "Arestas"],
    Densidade = net$metrics$Valor[net$metrics$Indicador == "Densidade"],
    Componentes = net$metrics$Valor[net$metrics$Indicador == "Componentes"],
    Clusters = net$metrics$Valor[net$metrics$Indicador == "Clusters"]
  )
}

plot_network_bundle <- function(net, main_title = "Rede") {
  validate(need(requireNamespace("igraph", quietly = TRUE), "Instale o pacote 'igraph' para habilitar redes e clusters."))
  validate(need(!is.null(net$graph), "Não houve dados suficientes para gerar essa rede."))

  g <- net$graph
  cl_n <- length(unique(igraph::V(g)$cluster))
  v_cols <- network_palette(cl_n)[match(igraph::V(g)$cluster, sort(unique(igraph::V(g)$cluster)))]
  v_size <- rescale_num(igraph::V(g)$strength, c(8, 24))
  e_w <- if (!is.null(igraph::E(g)$weight)) rescale_num(igraph::E(g)$weight, c(0.8, 5)) else 1.2
  lay <- igraph::layout_with_fr(g)

  plot(
    g,
    layout = lay,
    vertex.color = v_cols,
    vertex.size = v_size,
    vertex.frame.color = "#FFFFFF",
    vertex.label.color = "#203040",
    vertex.label.cex = ifelse(igraph::gorder(g) > 30, 0.55, 0.7),
    vertex.label.family = "sans",
    edge.color = grDevices::adjustcolor("#7F8C8D", alpha.f = 0.35),
    edge.width = e_w,
    main = main_title
  )
}


COL <- list(
  navy  = "#1F3864", blue  = "#1E40AF", teal  = "#047857",
  green = "#1E8449", amber = "#C2410C", red   = "#B91C1C",
  grey  = "#5D6D7E", lgrey = "#F2F3F4", white = "#FFFFFF",
  xblue = "#DEEAF1",
  p1dk  = "#1E40AF", p1lt  = "#DCEAF8",
  p2dk  = "#1E8449", p2lt  = "#DFF2E5",
  p3dk  = "#C2410C", p3lt  = "#FDEBD0",
  p4dk  = "#B91C1C", p4lt  = "#FADBD8"
)

default_criteria <- criteria_empty_df()

# ================================================================
# i18n — fonte única de tradução (PT/EN)
# A UI é uma função de `request`: o idioma é resolvido por requisição a partir
# de ?lang= na URL e cada string é construída via tr(). O servidor usa cur_lang()
# para textos dinâmicos (notificações, tabelas, gráficos, dashboard).
# Para estender: adicione uma chave em I18N$pt e a tradução correspondente em I18N$en.
# ================================================================
I18N <- list(
  pt = list(
    `header.action.new` = "Nova Análise",
    `brand.tagline` = "Literature Integration, Ranking & Analysis",
    `menu.bases` = "1. Tema e Bases", `menu.criteria` = "2. Critérios",
    `menu.run` = "3. Executar", `menu.dashboard` = "4. Dashboard",
    `menu.results` = "5. Resultados", `menu.queue` = "6. Fila semanal",
    `menu.articles` = "7. Artigos", `menu.export` = "8. Exportar",
    `menu.methodology` = "9. Metodologia",
    `status.label` = "Status",
    `status.waiting` = "Aguardando dados",
    `status.loaded` = "%d artigos carregados",
    `status.summary` = "%d artigos | %d P1",
    `chip.config` = "Configuração", `chip.optional` = "Opcional",
    `chip.classification` = "Classificação", `chip.reproducibility` = "Reprodutibilidade",
    `common.select_file` = "Selecionar arquivo", `common.no_file` = "Nenhum arquivo selecionado",
    `common.name` = "Nome", `common.type` = "Tipo", `common.format` = "Formato",
    `common.interpretation` = "Interpretação:", `common.highlight` = "Destaque:",

    # Aba 1
    `box.params` = "Parâmetros gerais",
    `title.search_range` = "Intervalo e data da busca",
    `note.params_use` = "Esses parâmetros são usados na análise, nos gráficos e no texto metodológico.",
    `label.search_date` = "Data da busca", `label.start_year` = "Ano inicial",
    `title.review_theme` = "Definir tema da revisão",
    `switch.enable_theme` = "Habilitar preenchimento do tema",
    `label.theme_title` = "Título do tema", `ph.theme_title` = "Ex: CRISPR-Cas9 in cancer therapy",
    `label.theme_desc` = "Descrição do tema (opcional)", `ph.theme_desc` = "Descreva o foco do review...",
    `label.lead_author` = "Autor principal", `ph.lead_author` = "Ex: Basílio LA",
    `label.institution` = "Instituição", `ph.institution` = "Ex: UFES, Vitória, ES, Brasil",
    `note.optional_section` = "Essa seção é opcional. Ative o preenchimento apenas se quiser personalizar a metodologia e identificar formalmente o projeto.",
    `box.sources` = "Selecionar bases de dados",
    `note.sources_pick` = "Selecione os arquivos exportados. Qualquer combinação de bases é aceita.",
    `src.pubmed.fmt` = "Formato: .txt ou .nbib", `src.wos.fmt` = "Formato: .txt (Plain Text)",
    `src.scopus.fmt` = "Formato: .bib, .ris ou .csv",
    `title.extra_db` = "Adicionar outra base",
    `switch.extra_db` = "Habilitar base adicional",
    `ph.extra_name` = "Ex: LILACS",
    `note.extra_db` = "Desabilitado por padrão. Ative apenas se quiser incluir uma base adicional além de PubMed, WoS e Scopus.",
    `btn.load_merge` = "Carregar e Mesclar Bases",
    `ibox.corpus_final` = "Corpus Final",

    # Aba 2
    `box.criteria` = "Configurar critérios de ranking",
    `note.criteria_intro` = "Defina aqui como o ranking será construído: eixo temático, tratamento de citações, uso de revistas e critérios temáticos.",
    `title.thematic_axis` = "Eixo temático",
    `choice.use_axis` = "Usar eixo temático", `choice.no_axis` = "Não usar eixo temático",
    `label.thematic_weight` = "Peso temático (%)",
    `note.axis_off` = "O ranking será calculado apenas com o componente bibliométrico.",
    `lbl.bib_score` = "Score bibliométrico:",
    `note.axis_components` = "Componente temático + componente bibliométrico",
    `note.bib_only` = "Modo somente bibliométrico",
    `title.citations` = "Tratamento de citações",
    `choice.cit_raw` = "Sem ponderação temporal", `choice.cit_weighted` = "Ponderar por citações/ano",
    `chk.recency` = "Incluir score de recência no componente bibliométrico",
    `note.citations` = "A opção ponderada reduz a vantagem automática de artigos muito antigos. A recência continua sendo um componente separado, quando ativada.",
    `title.journals` = "Revistas",
    `switch.journal_score` = "Usar score de revistas",
    `label.tier1` = "Tier 1 — Alto impacto (score 5)", `ph.tier1` = "Ex: NATURE|SCIENCE|CELL|NUCLEIC ACIDS|NAT METHODS",
    `label.tier2` = "Tier 2 — Impacto médio (score 4)", `ph.tier2` = "Ex: NAT REV|TRENDS|CURR OPIN|BIOINFORMATICS|NAT COMMUN",
    `label.tier3` = "Tier 3 — Impacto moderado (score 3)",
    `label.tier4` = "Tier 4 — Impacto básico (score 2)",
    `note.tiers` = "Revistas não enquadradas em nenhum tier recebem score 1. Use | como separador. Edite os tiers 3 e 4 conforme sua área de pesquisa.",
    `note.journals_off` = "Desabilitado por padrão. Ative apenas se quiser que o periódico influencie explicitamente o score bibliométrico.",
    `title.thematic_criteria` = "Critérios temáticos",
    `label.search_string` = "String de busca",
    `ph.search_string` = "Ex: (\"CRISPR\" OR \"Cas9\") AND (\"cancer\" OR \"tumor\") AND (\"delivery\" OR \"nanoparticles\")",
    `label.axis_mode` = "Como preencher o eixo temático",
    `choice.manual` = "Manual", `choice.from_string` = "Gerar pela string de busca", `choice.hybrid` = "String + manual",
    `note.axis_mode` = "Você pode construir os critérios do zero, gerar pela string ou combinar as duas abordagens.",
    `note.axis_off_criteria` = "O eixo temático está desativado. Ative-o para usar critérios manuais ou gerar critérios pela string de busca.",
    `chip.thematic_table` = "Tabela temática", `title.criteria_table` = "Tabela de critérios",
    `card.gen_from_string` = "Gerar critérios a partir da string de busca",
    `btn.replace_string` = "Substituir pela string", `btn.add_string` = "Somar à tabela atual", `btn.clear_criteria` = "Limpar critérios",
    `note.gen_string` = "Use a string para sugerir grupos temáticos e depois refine manualmente, se quiser.",
    `card.add_manual` = "Adicionar critério manualmente",
    `label.crit_name` = "Nome do critério", `ph.crit_name` = "Ex: Biodiversidade tropical",
    `label.crit_weight` = "Peso",
    `label.crit_kw` = "Palavras-chave (separadas por ;)", `ph.crit_kw` = "Ex: ATLANTIC FOREST; TROPICAL; BIODIVERSITY; CERRADO",
    `btn.add` = "Adicionar",
    `note.crit_kw` = "As palavras-chave são buscadas em TÍTULO + ABSTRACT + keywords do artigo.",
    `col.id` = "ID", `col.name` = "Nome", `col.weight` = "Peso", `col.keywords` = "Palavras-chave",
    `dt.empty_criteria` = "Nenhum critério adicionado.",

    # Aba 3
    `box.config` = "Configurações da análise",
    `title.thresholds` = "Defina os cortes de P1, P2, P3 e remoção",
    `label.p1` = "P1 (score ≥)", `label.p2` = "P2 (score ≥)", `label.p3` = "P3 (score ≥)", `label.removed` = "Removidos (score <)",
    `title.seed` = "Seed (Louvain)",
    `note.seed_explain` = "O algoritmo de Louvain tem componente estocástico. Fixar a semente garante que o mesmo resultado seja reproduzido ao re-executar o pipeline. Registre este valor na seção de métodos do manuscrito.",
    `box.execute` = "Executar pipeline completo",
    `title.pipeline` = "Pipeline de análise",
    `note.pipeline` = "Carregue as bases, configure os critérios e execute a análise completa.",
    `step1.t` = "Passo 1", `step1.s` = "Importar e mesclar bases",
    `step2.t` = "Passo 2", `step2.s` = "Análise bibliométrica",
    `step3.t` = "Passo 3", `step3.s` = "Aplicar cortes e classes",
    `step4.t` = "Passo 4", `step4.s` = "Gerar Excel + figuras",
    `btn.run_pipeline` = "Executar Pipeline Completo",
    `title.exec_log` = "Log de execução",

    # Aba 4
    `box.dashboard` = "Dashboard executivo",
    `box.corpus_indicators` = "Indicadores bibliométricos do corpus",
    `box.class_dist` = "Distribuição das classes",
    `box.annual_trend` = "Produção anual e tendência",
    `box.score_dist` = "Distribuição do score final",
    `box.top_p1` = "Top artigos P1",

    # Aba 5
    `box.biblio_analysis` = "Análise Bibliométrica",
    `tab.annual` = "Produção Anual", `tab.journals` = "Top Revistas", `tab.countries` = "Top Países",
    `tab.authors` = "Top Autores", `tab.keywords` = "Top Keywords", `tab.institutions` = "Top Instituições",
    `tab.cit_dist` = "Distribuição de citações", `tab.score_dist` = "Distribuição do score",
    `tab.score_scatter` = "Score temático × bibliométrico", `tab.kw_net` = "Rede de keywords",
    `tab.author_net` = "Rede de coautoria", `tab.country_net` = "Rede entre países",
    `tab.net_metrics` = "Métricas de rede", `tab.kw_clusters` = "Clusters de keywords", `tab.most_cited` = "Mais Citados",

    # Aba 6
    `box.queue` = "Fila semanal sugerida",
    `note.queue_intro` = "Sequência automática para leitura em seis blocos, com destaque explícito para Artigos fundadores, impacto, atualidade e síntese.",
    `label.week` = "Semana", `choice.all` = "Todas",
    `note.queue_rules` = "A fila considera apenas P1, P2 e P3. Artigos fundadores são identificados por alto impacto; reviews e revisões sistemáticas entram na semana 6; removidos não entram na fila.",

    # Aba 7
    `box.filters` = "Filtros", `label.class` = "Classe",
    `label.min_score` = "Score mínimo", `label.min_cit` = "Citações mínimas",
    `label.search_title_kw` = "Buscar no título/keywords", `ph.search_title_kw` = "Ex: marine, CRISPR, Atlantic...",

    # Aba 8
    `box.export_excel` = "Exportar Excel estilizado",
    `label.select_sheets` = "Selecione as abas:",
    `sheet.dashboard` = "Dashboard com indicadores", `sheet.p1` = "P1 - Leitura Obrigatoria",
    `sheet.p2` = "P2 - Leitura Recomendada", `sheet.p3` = "P3 - Opcional", `sheet.queue` = "Fila semanal",
    `sheet.biblio` = "Bibliometria robusta", `sheet.below` = "Abaixo do corte", `sheet.p4` = "P4 - Removidos",
    `sheet.prisma` = "PRISMA (numeros para manuscrito)", `sheet.template` = "Template de Anotacao",
    `sheet.ranking` = "Ranking Completo", `sheet.config` = "Configuracoes da Analise",
    `label.file_name` = "Nome do arquivo",
    `btn.download_excel` = "Baixar Excel Estilizado (.xlsx)",
    `box.export_figs` = "Exportar figuras (PNG 300 DPI)",
    `note.figs` = "Figuras geradas pelo pipeline:",
    `fig1` = "Fig 1 — Produção Anual", `fig2` = "Fig 2 — Top Revistas", `fig3` = "Fig 3 — Top Países",
    `fig4` = "Fig 4 — Top Autores", `fig5` = "Fig 5 — Top Keywords",

    # Aba 9
    `box.methodology` = "Texto de Metodologia para o Manuscrito",
    `note.methodology` = "Texto gerado automaticamente com base nas suas configurações.",
    `label.language` = "Idioma",
    `btn.gen_text` = "Gerar Texto", `btn.download_txt` = "Baixar como .txt",
    `ph.methodology` = "Clique em 'Gerar Texto' para criar o texto de metodologia com base nas configurações atuais.",

    # Modal nova análise
    `modal.reset_title` = "Iniciar nova análise?",
    `modal.reset_body` = "Todos os dados, arquivos carregados e resultados serão apagados.",
    `modal.reset_warn` = "Esta ação não pode ser desfeita.",
    `modal.cancel` = "Cancelar", `modal.reset_confirm` = "Sim, nova análise",

    # Notificações / dinâmico
    `notif.no_file` = "Selecione ao menos um arquivo!",
    `notif.loaded` = "%d artigos carregados!",
    `notif.no_data_run` = "Carregue e mescle ao menos uma base na aba '1. Tema e Bases' antes de executar o pipeline.",
    `notif.bad_thresholds` = "Ajuste os limiares para que P1 > P2 > P3 > Removidos ≥ 0 antes de executar.",
    `notif.no_criteria` = "O eixo temático está ativo, mas não há critérios temáticos definidos.",
    `notif.pipeline_ok` = "Pipeline executado com sucesso!",
    `notif.crit_added` = "Critério '%s' adicionado!",
    `notif.crit_extract_fail` = "Não foi possível extrair critérios da chave de busca.",
    `notif.crit_generated` = "Critérios gerados. <b>Atenção:</b> os pesos foram atribuídos automaticamente por ordem de aparição na string — revise-os antes de executar o pipeline.",
    `notif.crit_added_string` = "Critérios adicionados. <b>Atenção:</b> os pesos foram atribuídos automaticamente por ordem de aparição na string — revise-os antes de executar o pipeline.",
    `notif.crit_cleared` = "Critérios temáticos limpos.",
    `notif.manual_mode` = "Modo manual ativo. Adicione seus critérios temáticos na tabela.",
    `notif.metod_need_pipeline` = "Execute o pipeline antes de gerar o texto metodológico.",
    `notif.metod_ok` = "Texto gerado com sucesso!",
    `thr.valid` = "Ordem válida: <b>P1 &gt; P2 &gt; P3 &gt; Removidos</b>. Artigos abaixo do corte de remoção serão classificados como P4 - Removido; artigos entre esse corte e P3 ficam em 'Abaixo do corte'.",
    `thr.invalid` = "Ajuste os limiares para que P1 > P2 > P3 > Removidos ≥ 0.",
    `validate.thresholds` = "Os limiares devem obedecer a ordem P1 > P2 > P3 > Removidos ≥ 0.",
    `validate.igraph` = "Instale o pacote 'igraph' para habilitar redes e clusters.",
    `validate.no_network` = "Não houve dados suficientes para gerar essa rede.",
    `validate.no_inst` = "Sem dados institucionais suficientes para plotar.",

    # Info boxes (dashboard / resultados)
    `ib.p1` = "P1 - Obrigatória", `ib.p2` = "P2 - Recomendada", `ib.p3` = "P3 - Opcional",
    `ib.p4` = "P4 - Removidos", `ib.below` = "Abaixo do corte",
    `ib.score_ge` = "Score ≥ %s", `ib.score_range` = "Score %s–%s", `ib.score_lt` = "Score < %s",
    `dash.title_suffix` = "— Painel de Ranqueamento de Artigos",
    `dash.corpus` = "Corpus", `dash.articles` = "artigos", `dash.score` = "Score",
    `dash.score_combo` = "%d%% temático + %d%% bibliométrico", `dash.score_bib_only` = "100%% bibliométrico",

    # DT colnames
    `col.author` = "Autor", `col.year` = "Ano", `col.title` = "Título", `col.journal` = "Revista",
    `col.citations` = "Citações", `col.cit_per_year` = "Citações/ano", `col.score` = "Score",
    `col.priority` = "Classe", `col.doi` = "DOI", `col.week` = "Semana", `col.authors` = "Autores",

    # Indicadores
    `ind.total` = "Total artigos (após deduplicação)", `ind.bases` = "Bases incluídas",
    `ind.period` = "Período coberto", `ind.aagr` = "Taxa de crescimento anual (AAGR)",
    `ind.mean_cit` = "Média de citações por artigo", `ind.journals` = "Revistas únicas",
    `ind.top_country` = "País mais produtivo", `ind.top_journal` = "Revista mais produtiva",
    `ind.top_kw` = "Keyword mais frequente",
    `ind.col_indicator` = "Indicador", `ind.col_value` = "Valor", `ind.col_interp` = "Interpretação",
    `ind.i_total` = "Corpus final após deduplicação entre todas as bases configuradas",
    `ind.i_bases` = "Fontes atualmente carregadas no pipeline",
    `ind.i_period` = "Janela temporal coberta pelo corpus final",
    `ind.i_aagr` = "AAGR (média aritmética das taxas anuais) calculada sobre a produção anual",
    `ind.i_mean_cit` = "Impacto médio do corpus com base nas citações disponíveis",
    `ind.i_journals` = "Diversidade de veículos de publicação",
    `ind.i_country` = "País com maior produção segundo afiliações",
    `ind.i_journal` = "Periódico mais frequente no corpus",
    `ind.i_kw` = "Tema central mais recorrente nas keywords",

    # Interpretações
    `interp.annual` = "A produção foi de %d artigos em %d e de %d artigos em %d.",
    `interp.journals` = "A revista mais produtiva no corpus é %s.",
    `interp.journals_none` = "Não foi possível identificar um periódico dominante.",
    `interp.countries` = "Brasil contribui com %s%% (%d de %d artigos).",
    `interp.countries_none` = "Dados de país não disponíveis neste corpus.",
    `interp.keywords` = "As keywords mostram os clusters temáticos dominantes e ajudam a refinar o eixo temático do ranking.",
    `fila.title` = "Fila semanal (%d artigos)",
    `art.title` = "Artigos filtrados (%d)",

    # Semanas
    `wk1` = "Semana 1 - Artigos fundadores", `wk2` = "Semana 2 - Obrigatórios recentes",
    `wk3` = "Semana 3 - Recomendados de maior impacto", `wk4` = "Semana 4 - Recomendados complementares",
    `wk5` = "Semana 5 - Opcionais estratégicos", `wk6` = "Semana 6 - Reviews e sínteses",

    # Plots
    `plot.annual.title` = "Produção Científica Anual", `plot.annual.x` = "Ano", `plot.annual.y` = "Publicações",
    `plot.annual.sub` = "n=%d artigos — %s",
    `plot.journals.title` = "Top 20 Revistas", `plot.countries.title` = "Top Países",
    `plot.countries.sub` = "Verde = Brasil", `plot.authors.title` = "Top 20 Autores",
    `plot.keywords.title` = "Top 25 Keywords", `plot.institutions.title` = "Top 20 Instituições",
    `plot.cit_dist.title` = "Distribuição de Citações", `plot.cit_dist.x` = "Citações", `plot.cit_dist.y` = "Número de artigos",
    `plot.score_dist.title` = "Distribuição do Score Final", `plot.score_dist.x` = "Score final", `plot.score_dist.y` = "Número de artigos",
    `plot.scatter.title` = "Score Temático × Score Bibliométrico", `plot.scatter.x` = "Score temático", `plot.scatter.y` = "Score bibliométrico",
    `plot.class.y` = "Número de artigos",
    `plot.kw_net` = "Rede de coocorrência de keywords", `plot.author_net` = "Rede de coautoria", `plot.country_net` = "Rede de colaboração entre países"
  ),
  en = list(
    `header.action.new` = "New Analysis",
    `brand.tagline` = "Literature Integration, Ranking & Analysis",
    `menu.bases` = "1. Topic & Sources", `menu.criteria` = "2. Criteria",
    `menu.run` = "3. Run", `menu.dashboard` = "4. Dashboard",
    `menu.results` = "5. Results", `menu.queue` = "6. Reading queue",
    `menu.articles` = "7. Articles", `menu.export` = "8. Export",
    `menu.methodology` = "9. Methodology",
    `status.label` = "Status",
    `status.waiting` = "Waiting for data",
    `status.loaded` = "%d articles loaded",
    `status.summary` = "%d articles | %d P1",
    `chip.config` = "Configuration", `chip.optional` = "Optional",
    `chip.classification` = "Classification", `chip.reproducibility` = "Reproducibility",
    `common.select_file` = "Select file", `common.no_file` = "No file selected",
    `common.name` = "Name", `common.type` = "Type", `common.format` = "Format",
    `common.interpretation` = "Interpretation:", `common.highlight` = "Highlight:",

    `box.params` = "General parameters",
    `title.search_range` = "Search range and date",
    `note.params_use` = "These parameters are used in the analysis, charts and methods text.",
    `label.search_date` = "Search date", `label.start_year` = "Start year",
    `title.review_theme` = "Define review topic",
    `switch.enable_theme` = "Enable topic filling",
    `label.theme_title` = "Topic title", `ph.theme_title` = "e.g., CRISPR-Cas9 in cancer therapy",
    `label.theme_desc` = "Topic description (optional)", `ph.theme_desc` = "Describe the review focus...",
    `label.lead_author` = "Lead author", `ph.lead_author` = "e.g., Basilio LA",
    `label.institution` = "Institution", `ph.institution` = "e.g., UFES, Vitoria, ES, Brazil",
    `note.optional_section` = "This section is optional. Enable filling only if you want to customize the methodology and formally identify the project.",
    `box.sources` = "Select databases",
    `note.sources_pick` = "Select the exported files. Any combination of databases is accepted.",
    `src.pubmed.fmt` = "Format: .txt or .nbib", `src.wos.fmt` = "Format: .txt (Plain Text)",
    `src.scopus.fmt` = "Format: .bib, .ris or .csv",
    `title.extra_db` = "Add another database",
    `switch.extra_db` = "Enable additional database",
    `ph.extra_name` = "e.g., LILACS",
    `note.extra_db` = "Disabled by default. Enable only if you want to include a database beyond PubMed, WoS and Scopus.",
    `btn.load_merge` = "Load & Merge Databases",
    `ibox.corpus_final` = "Final Corpus",

    `box.criteria` = "Configure ranking criteria",
    `note.criteria_intro` = "Define how the ranking is built: thematic axis, citation handling, journal scoring and thematic criteria.",
    `title.thematic_axis` = "Thematic axis",
    `choice.use_axis` = "Use thematic axis", `choice.no_axis` = "No thematic axis",
    `label.thematic_weight` = "Thematic weight (%)",
    `note.axis_off` = "The ranking will be computed from the bibliometric component only.",
    `lbl.bib_score` = "Bibliometric score:",
    `note.axis_components` = "Thematic component + bibliometric component",
    `note.bib_only` = "Bibliometric-only mode",
    `title.citations` = "Citation handling",
    `choice.cit_raw` = "No temporal weighting", `choice.cit_weighted` = "Weight by citations/year",
    `chk.recency` = "Include recency score in the bibliometric component",
    `note.citations` = "The weighted option reduces the automatic advantage of much older articles. Recency remains a separate component when enabled.",
    `title.journals` = "Journals",
    `switch.journal_score` = "Use journal score",
    `label.tier1` = "Tier 1 — High impact (score 5)", `ph.tier1` = "e.g., NATURE|SCIENCE|CELL|NUCLEIC ACIDS|NAT METHODS",
    `label.tier2` = "Tier 2 — Medium impact (score 4)", `ph.tier2` = "e.g., NAT REV|TRENDS|CURR OPIN|BIOINFORMATICS|NAT COMMUN",
    `label.tier3` = "Tier 3 — Moderate impact (score 3)",
    `label.tier4` = "Tier 4 — Basic impact (score 2)",
    `note.tiers` = "Journals not matched in any tier receive score 1. Use | as the separator. Edit tiers 3 and 4 to fit your research area.",
    `note.journals_off` = "Disabled by default. Enable only if you want the journal to explicitly influence the bibliometric score.",
    `title.thematic_criteria` = "Thematic criteria",
    `label.search_string` = "Search string",
    `ph.search_string` = "e.g., (\"CRISPR\" OR \"Cas9\") AND (\"cancer\" OR \"tumor\") AND (\"delivery\" OR \"nanoparticles\")",
    `label.axis_mode` = "How to fill the thematic axis",
    `choice.manual` = "Manual", `choice.from_string` = "Generate from search string", `choice.hybrid` = "String + manual",
    `note.axis_mode` = "You can build criteria from scratch, generate them from the string, or combine both approaches.",
    `note.axis_off_criteria` = "The thematic axis is disabled. Enable it to use manual criteria or to generate criteria from the search string.",
    `chip.thematic_table` = "Thematic table", `title.criteria_table` = "Criteria table",
    `card.gen_from_string` = "Generate criteria from the search string",
    `btn.replace_string` = "Replace from string", `btn.add_string` = "Add to current table", `btn.clear_criteria` = "Clear criteria",
    `note.gen_string` = "Use the string to suggest thematic groups, then refine them manually if you wish.",
    `card.add_manual` = "Add criterion manually",
    `label.crit_name` = "Criterion name", `ph.crit_name` = "e.g., Tropical biodiversity",
    `label.crit_weight` = "Weight",
    `label.crit_kw` = "Keywords (separated by ;)", `ph.crit_kw` = "e.g., ATLANTIC FOREST; TROPICAL; BIODIVERSITY; CERRADO",
    `btn.add` = "Add",
    `note.crit_kw` = "Keywords are searched in TITLE + ABSTRACT + article keywords.",
    `col.id` = "ID", `col.name` = "Name", `col.weight` = "Weight", `col.keywords` = "Keywords",
    `dt.empty_criteria` = "No criteria added.",

    `box.config` = "Analysis settings",
    `title.thresholds` = "Set cutoffs for P1, P2, P3 and removal",
    `label.p1` = "P1 (score ≥)", `label.p2` = "P2 (score ≥)", `label.p3` = "P3 (score ≥)", `label.removed` = "Removed (score <)",
    `title.seed` = "Seed (Louvain)",
    `note.seed_explain` = "The Louvain algorithm has a stochastic component. Fixing the seed guarantees the same result when re-running the pipeline. Record this value in the methods section of the manuscript.",
    `box.execute` = "Run complete pipeline",
    `title.pipeline` = "Analysis pipeline",
    `note.pipeline` = "Load the databases, configure the criteria and run the complete analysis.",
    `step1.t` = "Step 1", `step1.s` = "Import and merge databases",
    `step2.t` = "Step 2", `step2.s` = "Bibliometric analysis",
    `step3.t` = "Step 3", `step3.s` = "Apply cutoffs and classes",
    `step4.t` = "Step 4", `step4.s` = "Generate Excel + figures",
    `btn.run_pipeline` = "Run Complete Pipeline",
    `title.exec_log` = "Execution log",

    `box.dashboard` = "Executive dashboard",
    `box.corpus_indicators` = "Corpus bibliometric indicators",
    `box.class_dist` = "Class distribution",
    `box.annual_trend` = "Annual production and trend",
    `box.score_dist` = "Final score distribution",
    `box.top_p1` = "Top P1 articles",

    `box.biblio_analysis` = "Bibliometric Analysis",
    `tab.annual` = "Annual Production", `tab.journals` = "Top Journals", `tab.countries` = "Top Countries",
    `tab.authors` = "Top Authors", `tab.keywords` = "Top Keywords", `tab.institutions` = "Top Institutions",
    `tab.cit_dist` = "Citation distribution", `tab.score_dist` = "Score distribution",
    `tab.score_scatter` = "Thematic × bibliometric score", `tab.kw_net` = "Keyword network",
    `tab.author_net` = "Co-authorship network", `tab.country_net` = "Country network",
    `tab.net_metrics` = "Network metrics", `tab.kw_clusters` = "Keyword clusters", `tab.most_cited` = "Most Cited",

    `box.queue` = "Suggested weekly queue",
    `note.queue_intro` = "Automatic six-block reading sequence, explicitly highlighting founding articles, impact, recency and synthesis.",
    `label.week` = "Week", `choice.all` = "All",
    `note.queue_rules` = "The queue considers only P1, P2 and P3. Founding articles are identified by high impact; reviews and systematic reviews go to week 6; removed articles are excluded.",

    `box.filters` = "Filters", `label.class` = "Class",
    `label.min_score` = "Minimum score", `label.min_cit` = "Minimum citations",
    `label.search_title_kw` = "Search in title/keywords", `ph.search_title_kw` = "e.g., marine, CRISPR, Atlantic...",

    `box.export_excel` = "Export styled Excel",
    `label.select_sheets` = "Select sheets:",
    `sheet.dashboard` = "Dashboard with indicators", `sheet.p1` = "P1 - Mandatory Reading",
    `sheet.p2` = "P2 - Recommended Reading", `sheet.p3` = "P3 - Optional", `sheet.queue` = "Weekly queue",
    `sheet.biblio` = "Robust bibliometrics", `sheet.below` = "Below cutoff", `sheet.p4` = "P4 - Removed",
    `sheet.prisma` = "PRISMA (numbers for manuscript)", `sheet.template` = "Annotation template",
    `sheet.ranking` = "Full ranking", `sheet.config` = "Analysis settings",
    `label.file_name` = "File name",
    `btn.download_excel` = "Download Styled Excel (.xlsx)",
    `box.export_figs` = "Export figures (PNG 300 DPI)",
    `note.figs` = "Figures generated by the pipeline:",
    `fig1` = "Fig 1 — Annual Production", `fig2` = "Fig 2 — Top Journals", `fig3` = "Fig 3 — Top Countries",
    `fig4` = "Fig 4 — Top Authors", `fig5` = "Fig 5 — Top Keywords",

    `box.methodology` = "Methodology Text for the Manuscript",
    `note.methodology` = "Text generated automatically from your settings.",
    `label.language` = "Language",
    `btn.gen_text` = "Generate Text", `btn.download_txt` = "Download as .txt",
    `ph.methodology` = "Click 'Generate Text' to create the methodology text from the current settings.",

    `modal.reset_title` = "Start a new analysis?",
    `modal.reset_body` = "All data, loaded files and results will be erased.",
    `modal.reset_warn` = "This action cannot be undone.",
    `modal.cancel` = "Cancel", `modal.reset_confirm` = "Yes, new analysis",

    `notif.no_file` = "Select at least one file!",
    `notif.loaded` = "%d articles loaded!",
    `notif.no_data_run` = "Load and merge at least one database in the '1. Topic & Sources' tab before running the pipeline.",
    `notif.bad_thresholds` = "Adjust the cutoffs so that P1 > P2 > P3 > Removed ≥ 0 before running.",
    `notif.no_criteria` = "The thematic axis is enabled, but no thematic criteria are defined.",
    `notif.pipeline_ok` = "Pipeline executed successfully!",
    `notif.crit_added` = "Criterion '%s' added!",
    `notif.crit_extract_fail` = "Could not extract criteria from the search string.",
    `notif.crit_generated` = "Criteria generated. <b>Note:</b> weights were assigned automatically by order of appearance in the string — review them before running the pipeline.",
    `notif.crit_added_string` = "Criteria added. <b>Note:</b> weights were assigned automatically by order of appearance in the string — review them before running the pipeline.",
    `notif.crit_cleared` = "Thematic criteria cleared.",
    `notif.manual_mode` = "Manual mode active. Add your thematic criteria in the table.",
    `notif.metod_need_pipeline` = "Run the pipeline before generating the methodology text.",
    `notif.metod_ok` = "Text generated successfully!",
    `thr.valid` = "Valid order: <b>P1 &gt; P2 &gt; P3 &gt; Removed</b>. Articles below the removal cutoff are classified as P4 - Removed; articles between that cutoff and P3 fall under 'Below cutoff'.",
    `thr.invalid` = "Adjust the cutoffs so that P1 > P2 > P3 > Removed ≥ 0.",
    `validate.thresholds` = "Cutoffs must follow the order P1 > P2 > P3 > Removed ≥ 0.",
    `validate.igraph` = "Install the 'igraph' package to enable networks and clusters.",
    `validate.no_network` = "Not enough data to generate this network.",
    `validate.no_inst` = "Not enough institutional data to plot.",

    `ib.p1` = "P1 - Mandatory", `ib.p2` = "P2 - Recommended", `ib.p3` = "P3 - Optional",
    `ib.p4` = "P4 - Removed", `ib.below` = "Below cutoff",
    `ib.score_ge` = "Score ≥ %s", `ib.score_range` = "Score %s–%s", `ib.score_lt` = "Score < %s",
    `dash.title_suffix` = "— Article Ranking Dashboard",
    `dash.corpus` = "Corpus", `dash.articles` = "articles", `dash.score` = "Score",
    `dash.score_combo` = "%d%% thematic + %d%% bibliometric", `dash.score_bib_only` = "100%% bibliometric",

    `col.author` = "Author", `col.year` = "Year", `col.title` = "Title", `col.journal` = "Journal",
    `col.citations` = "Citations", `col.cit_per_year` = "Citations/year", `col.score` = "Score",
    `col.priority` = "Class", `col.doi` = "DOI", `col.week` = "Week", `col.authors` = "Authors",

    `ind.total` = "Total articles (after deduplication)", `ind.bases` = "Databases included",
    `ind.period` = "Period covered", `ind.aagr` = "Average annual growth rate (AAGR)",
    `ind.mean_cit` = "Mean citations per article", `ind.journals` = "Unique journals",
    `ind.top_country` = "Most productive country", `ind.top_journal` = "Most productive journal",
    `ind.top_kw` = "Most frequent keyword",
    `ind.col_indicator` = "Indicator", `ind.col_value` = "Value", `ind.col_interp` = "Interpretation",
    `ind.i_total` = "Final corpus after deduplication across all configured databases",
    `ind.i_bases` = "Sources currently loaded in the pipeline",
    `ind.i_period` = "Time window covered by the final corpus",
    `ind.i_aagr` = "AAGR (arithmetic mean of annual rates) computed over annual production",
    `ind.i_mean_cit` = "Mean impact of the corpus based on available citations",
    `ind.i_journals` = "Diversity of publication venues",
    `ind.i_country` = "Country with highest production by affiliations",
    `ind.i_journal` = "Most frequent journal in the corpus",
    `ind.i_kw` = "Most recurrent central topic in keywords",

    `interp.annual` = "Production was %d articles in %d and %d articles in %d.",
    `interp.journals` = "The most productive journal in the corpus is %s.",
    `interp.journals_none` = "Could not identify a dominant journal.",
    `interp.countries` = "Brazil contributes %s%% (%d of %d articles).",
    `interp.countries_none` = "Country data not available in this corpus.",
    `interp.keywords` = "Keywords reveal the dominant thematic clusters and help refine the thematic axis of the ranking.",
    `fila.title` = "Weekly queue (%d articles)",
    `art.title` = "Filtered articles (%d)",

    `wk1` = "Week 1 - Founding articles", `wk2` = "Week 2 - Recent mandatory",
    `wk3` = "Week 3 - Higher-impact recommended", `wk4` = "Week 4 - Complementary recommended",
    `wk5` = "Week 5 - Strategic optional", `wk6` = "Week 6 - Reviews and syntheses",

    `plot.annual.title` = "Annual Scientific Production", `plot.annual.x` = "Year", `plot.annual.y` = "Publications",
    `plot.annual.sub` = "n=%d articles — %s",
    `plot.journals.title` = "Top 20 Journals", `plot.countries.title` = "Top Countries",
    `plot.countries.sub` = "Green = Brazil", `plot.authors.title` = "Top 20 Authors",
    `plot.keywords.title` = "Top 25 Keywords", `plot.institutions.title` = "Top 20 Institutions",
    `plot.cit_dist.title` = "Citation Distribution", `plot.cit_dist.x` = "Citations", `plot.cit_dist.y` = "Number of articles",
    `plot.score_dist.title` = "Final Score Distribution", `plot.score_dist.x` = "Final score", `plot.score_dist.y` = "Number of articles",
    `plot.scatter.title` = "Thematic Score × Bibliometric Score", `plot.scatter.x` = "Thematic score", `plot.scatter.y` = "Bibliometric score",
    `plot.class.y` = "Number of articles",
    `plot.kw_net` = "Keyword co-occurrence network", `plot.author_net` = "Co-authorship network", `plot.country_net` = "Country collaboration network"
  )
)

normalize_lang <- function(lang) {
  if (length(lang) == 0 || is.null(lang) || is.na(lang[1]) || !lang[1] %in% c("pt", "en")) "pt" else lang[1]
}

# Tradutor de tempo de construção (UI). Retorna a string crua para a chave/idioma.
make_tr <- function(lang) {
  lang <- normalize_lang(lang)
  function(key) {
    v <- I18N[[lang]][[key]]
    if (is.null(v)) v <- I18N[["pt"]][[key]]
    if (is.null(v)) v <- key
    v
  }
}

lang_from_request <- function(request) {
  qs <- tryCatch(shiny::parseQueryString(request$QUERY_STRING), error = function(e) list())
  normalize_lang(qs[["lang"]])
}

# ================================================================
# UI
# ================================================================
ui <- function(request) {
  tr   <- make_tr(lang_from_request(request))
  lang <- normalize_lang(lang_from_request(request))
  dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$div(
      class = "lira-brand",
      tags$div(class = "lira-mark"),
      tags$div(
        class = "lira-wordmark",
        tags$span(class = "lira-name", "LIRA"),
        tags$span(class = "lira-tag", tr("brand.tagline"))
      ),
      tags$span(class = "lira-version", "v1.0.0")
    ),
    titleWidth = 420,
    tags$li(
      class = "dropdown",
      tags$div(
        class = "lira-lang-switch",
        tags$button(class = paste("lira-lang-btn", if (lang == "pt") "active" else ""), `data-lang` = "pt", onclick = "liraSetLang('pt')", "PT"),
        tags$button(class = paste("lira-lang-btn", if (lang == "en") "active" else ""), `data-lang` = "en", onclick = "liraSetLang('en')", "EN")
      )
    ),
    tags$li(
      class = "dropdown",
      actionLink(
        "btn_nova_analise",
        tags$span(icon("rotate-right"), " ", tags$span(tr("header.action.new"))),
        class = "lira-header-action"
      )
    )
  ),

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem(tr("menu.bases"),       tabName = "bases",         icon = icon("database")),
      menuItem(tr("menu.criteria"),    tabName = "criterios",     icon = icon("sliders")),
      menuItem(tr("menu.run"),         tabName = "pipeline",      icon = icon("play")),
      menuItem(tr("menu.dashboard"),   tabName = "dashboard",     icon = icon("tachometer-alt")),
      menuItem(tr("menu.results"),     tabName = "resultados",    icon = icon("chart-bar")),
      menuItem(tr("menu.queue"),       tabName = "fila_semanal",  icon = icon("calendar-alt")),
      menuItem(tr("menu.articles"),    tabName = "artigos",       icon = icon("list-ol")),
      menuItem(tr("menu.export"),      tabName = "exportar",      icon = icon("file-excel")),
      menuItem(tr("menu.methodology"), tabName = "metodologia",   icon = icon("file-alt")),
      hr(),
      tags$div(
        class = "sidebar-status",
        tags$b(tr("status.label")),
        uiOutput("status_sidebar")
      )
    )
  ),

  dashboardBody(
    useWaiter(),

    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,400;9..144,500;9..144,600;9..144,700&family=Plus+Jakarta+Sans:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"),
      tags$style(HTML("
      /* ═══════════════════════════════════════════════════════════
         LIRA · Design System v1
         Editorial scientific — refined, calm, intentional
      ═══════════════════════════════════════════════════════════ */
      :root {
        --ink:        #0B1F3F;
        --ink-soft:   #1A2B45;
        --ink-mute:   #475569;
        --ink-faint:  #94A3B8;
        --paper:      #FAFAF7;
        --paper-soft: #F5F2EC;
        --paper-card: #FFFFFF;
        --line:       #E8E4DA;
        --line-soft:  #F0EDE5;
        --accent:     #C2410C;
        --accent-hi:  #D97706;
        --accent-soft:#FEF3C7;
        --success:    #047857;
        --success-soft:#D1FAE5;
        --danger:     #B91C1C;
        --danger-soft:#FEE2E2;
        --info:       #1E40AF;
        --info-soft:  #DBEAFE;

        --font-display: 'Fraunces', Georgia, 'Times New Roman', serif;
        --font-sans:    'Plus Jakarta Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        --font-mono:    'JetBrains Mono', ui-monospace, 'SF Mono', Menlo, monospace;

        --radius-sm: 8px;
        --radius:    12px;
        --radius-lg: 16px;
        --shadow-sm: 0 1px 2px rgba(11,31,63,0.04), 0 1px 1px rgba(11,31,63,0.03);
        --shadow:    0 4px 16px rgba(11,31,63,0.06), 0 1px 3px rgba(11,31,63,0.04);
        --shadow-lg: 0 16px 40px rgba(11,31,63,0.08), 0 4px 12px rgba(11,31,63,0.05);
      }

      /* ── Base typography ────────────────────────────────────────── */
      body, .wrapper, .content-wrapper, .right-side {
        font-family: var(--font-sans) !important;
        font-feature-settings: 'cv11', 'ss01';
        color: var(--ink-soft);
        background: var(--paper) !important;
      }
      h1, h2, h3, h4, h5, h6, .box-title { font-family: var(--font-sans); letter-spacing: -0.01em; }

      /* ═══ HEADER & BRAND ════════════════════════════════════════ */
      .main-header { box-shadow: none; }
      .main-header .navbar,
      .main-header .logo {
        background: var(--ink) !important;
        border-bottom: 1px solid rgba(255,255,255,0.06);
        height: 64px !important;
        line-height: 64px !important;
      }
      .main-header .logo { width: 420px !important; padding: 0 !important; }
      .main-header .navbar { margin-left: 420px !important; min-height: 64px; }
      .main-header .sidebar-toggle { color: rgba(255,255,255,0.65); }
      .main-header .sidebar-toggle:hover { background: rgba(255,255,255,0.06); color: white; }

      .lira-brand {
        display: flex; align-items: center; gap: 14px;
        padding: 0 22px; height: 64px;
        font-family: var(--font-sans);
      }
      .lira-mark {
        width: 30px; height: 30px; flex-shrink: 0;
        background:
          radial-gradient(circle at 30% 30%, var(--accent-hi) 0%, var(--accent) 60%, transparent 70%),
          radial-gradient(circle at 70% 70%, var(--info) 0%, transparent 55%);
        border-radius: 8px;
        position: relative;
        box-shadow: inset 0 0 0 1px rgba(255,255,255,0.15);
      }
      .lira-mark::after {
        content: ''; position: absolute; inset: 6px;
        border: 1.5px solid rgba(255,255,255,0.85);
        border-radius: 4px;
        border-right-color: transparent;
        border-bottom-color: transparent;
        transform: rotate(-12deg);
      }
      .lira-wordmark { display: flex; flex-direction: column; line-height: 1.1; }
      .lira-name {
        font-family: var(--font-display);
        font-weight: 600;
        font-size: 22px;
        font-variation-settings: 'opsz' 144, 'SOFT' 50;
        color: #FAFAF7;
        letter-spacing: -0.01em;
      }
      .lira-tag {
        font-size: 10.5px;
        font-weight: 500;
        color: rgba(250,250,247,0.55);
        letter-spacing: 0.04em;
        text-transform: uppercase;
        margin-top: 1px;
      }
      .lira-version {
        margin-left: auto;
        font-family: var(--font-mono);
        font-size: 10.5px;
        color: rgba(250,250,247,0.75);
        background: rgba(255,255,255,0.08);
        border: 1px solid rgba(255,255,255,0.12);
        padding: 3px 8px;
        border-radius: 999px;
      }
      .lira-header-action {
        color: rgba(255,255,255,0.85) !important;
        padding: 0 18px !important;
        font-size: 13px !important;
        font-weight: 500;
        line-height: 64px !important;
        height: 64px;
        display: inline-block;
        cursor: pointer;
        transition: all 0.15s ease;
      }
      .lira-header-action:hover { background: rgba(255,255,255,0.06); color: white !important; }

      /* ═══ SIDEBAR ══════════════════════════════════════════════ */
      .main-sidebar, .left-side {
        background: var(--ink) !important;
        box-shadow: inset -1px 0 0 rgba(255,255,255,0.04);
      }
      .sidebar-menu { padding: 18px 12px 0 12px; }
      .sidebar-menu > li > a {
        font-family: var(--font-sans) !important;
        font-size: 13.5px !important;
        font-weight: 500;
        color: rgba(255,255,255,0.6) !important;
        border-radius: 10px !important;
        padding: 11px 14px !important;
        margin-bottom: 2px;
        transition: all 0.15s ease;
        border-left: 0 !important;
      }
      .sidebar-menu > li > a:hover {
        background: rgba(255,255,255,0.05) !important;
        color: rgba(255,255,255,0.95) !important;
      }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li.active > a:hover {
        background: rgba(217,119,6,0.12) !important;
        color: #FED7AA !important;
        border-left: 0 !important;
        box-shadow: inset 2px 0 0 var(--accent-hi);
      }
      .sidebar-menu > li > a > .fa,
      .sidebar-menu > li > a > .fas,
      .sidebar-menu > li > a > i {
        font-size: 13px !important;
        width: 18px;
        margin-right: 10px;
        opacity: 0.85;
      }
      .sidebar-menu > li.header { display: none; }
      .main-sidebar hr {
        border-color: rgba(255,255,255,0.08);
        margin: 14px 18px;
      }

      /* ═══ CONTENT AREA ════════════════════════════════════════ */
      .content-wrapper { background: var(--paper) !important; padding-top: 8px; }
      .content { padding: 18px 22px; }

      /* ═══ BOXES (cards Shinydashboard) ════════════════════════ */
      .box {
        background: var(--paper-card) !important;
        border-radius: var(--radius-lg) !important;
        box-shadow: var(--shadow) !important;
        border: 1px solid var(--line-soft) !important;
        overflow: hidden;
        margin-bottom: 18px;
      }
      .box.box-solid.box-primary,
      .box.box-solid.box-warning,
      .box.box-solid.box-info,
      .box.box-solid.box-success,
      .box.box-solid.box-danger {
        border: 1px solid var(--line-soft) !important;
      }
      .box.box-solid.box-primary > .box-header,
      .box.box-solid > .box-header {
        background: transparent !important;
        color: var(--ink) !important;
        border-bottom: 1px solid var(--line-soft) !important;
        padding: 16px 22px !important;
      }
      .box-header .box-title {
        font-family: var(--font-display) !important;
        font-weight: 500 !important;
        font-size: 18px !important;
        color: var(--ink) !important;
        letter-spacing: -0.015em;
        font-variation-settings: 'opsz' 144;
      }
      .box-body { padding: 18px 22px 22px !important; }

      /* Substitui a barra superior colorida dos boxes pela acentuação lateral elegante */
      .box.box-solid.box-primary { border-top: 3px solid var(--info) !important; border-top-left-radius: var(--radius-lg) !important; border-top-right-radius: var(--radius-lg) !important; }
      .box.box-solid.box-warning { border-top: 3px solid var(--accent) !important; }
      .box.box-solid.box-info    { border-top: 3px solid var(--info) !important; }
      .box.box-solid.box-success { border-top: 3px solid var(--success) !important; }
      .box.box-solid.box-danger  { border-top: 3px solid var(--danger) !important; }

      /* ═══ CARDS INTERNOS (clean-card, db-card, etc) ═══════════ */
      .db-card, .clean-card {
        background: var(--paper-card);
        border-radius: var(--radius);
        padding: 18px;
        border: 1px solid var(--line-soft);
        margin-bottom: 12px;
        box-shadow: var(--shadow-sm);
        transition: all 0.18s ease;
      }
      .clean-card.soft {
        background: var(--paper-soft);
        border-color: var(--line);
      }
      .db-card:hover { border-color: var(--line); box-shadow: var(--shadow); }
      .criteria-card, .subsection-card {
        background: var(--paper-card);
        border-radius: var(--radius);
        padding: 16px;
        margin-bottom: 12px;
        border: 1px solid var(--line-soft);
        box-shadow: var(--shadow-sm);
      }

      /* ═══ SECTION CHIPS & TITLES ═══════════════════════════════ */
      .section-chip {
        display: inline-block;
        padding: 3px 9px;
        border-radius: 4px;
        background: var(--paper-soft);
        color: var(--ink-mute);
        font-family: var(--font-mono);
        font-size: 10px;
        font-weight: 500;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        margin-bottom: 12px;
        border: 1px solid var(--line);
      }
      .sec-title {
        font-family: var(--font-display);
        font-size: 19px;
        font-weight: 500;
        color: var(--ink);
        margin-bottom: 12px;
        letter-spacing: -0.015em;
        font-variation-settings: 'opsz' 144;
        line-height: 1.25;
      }
      .minor-note {
        color: var(--ink-mute);
        font-size: 12px;
        line-height: 1.6;
      }
      .disabled-note {
        background: var(--paper-soft);
        border: 1px dashed var(--line);
        color: var(--ink-mute);
        border-radius: var(--radius);
        padding: 14px 16px;
        font-size: 12.5px;
        line-height: 1.55;
      }

      /* ═══ INPUTS & FORMS ═════════════════════════════════════ */
      .form-control, .selectize-input {
        font-family: var(--font-sans) !important;
        border-radius: var(--radius-sm) !important;
        border: 1px solid var(--line) !important;
        background: var(--paper-card) !important;
        color: var(--ink-soft) !important;
        font-size: 13px !important;
        padding: 9px 12px !important;
        height: 38px !important;
        box-shadow: none !important;
        transition: all 0.15s ease;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: var(--accent) !important;
        box-shadow: 0 0 0 3px rgba(194,65,12,0.12) !important;
      }
      label, .control-label {
        font-family: var(--font-sans);
        font-size: 12px !important;
        font-weight: 600 !important;
        color: var(--ink-soft) !important;
        text-transform: none;
        letter-spacing: 0;
        margin-bottom: 6px !important;
      }
      textarea.form-control { height: auto !important; min-height: 60px; line-height: 1.5; }

      /* ═══ BUTTONS ════════════════════════════════════════════ */
      .btn {
        font-family: var(--font-sans) !important;
        font-weight: 600 !important;
        border-radius: var(--radius-sm) !important;
        border: 0 !important;
        font-size: 13px !important;
        letter-spacing: 0 !important;
        transition: all 0.15s ease;
        box-shadow: var(--shadow-sm);
      }
      .btn:hover { transform: translateY(-1px); box-shadow: var(--shadow); }
      .btn-default {
        background: var(--paper-card) !important;
        color: var(--ink-soft) !important;
        border: 1px solid var(--line) !important;
      }
      .btn-primary { background: var(--ink) !important; color: white !important; }
      .btn-primary:hover { background: var(--ink-soft) !important; }
      .btn-success { background: var(--success) !important; color: white !important; }
      .btn-warning { background: var(--accent) !important; color: white !important; }
      .btn-info    { background: var(--info) !important; color: white !important; }
      .btn-danger  { background: var(--danger) !important; color: white !important; }

      /* ═══ DATATABLES ════════════════════════════════════════ */
      table.dataTable {
        font-family: var(--font-sans) !important;
        font-size: 12.5px !important;
        border-collapse: separate !important;
      }
      table.dataTable thead th {
        font-weight: 600 !important;
        color: var(--ink-soft) !important;
        background: var(--paper-soft) !important;
        border-bottom: 1px solid var(--line) !important;
        font-size: 11.5px !important;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        padding: 10px 12px !important;
      }
      table.dataTable tbody td {
        padding: 9px 12px !important;
        border-bottom: 1px solid var(--line-soft) !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: var(--ink) !important;
        color: white !important;
        border: 0 !important;
        border-radius: 6px !important;
      }

      /* ═══ MISC ════════════════════════════════════════════════ */
      .metod-box {
        background: var(--paper-soft);
        border-radius: var(--radius);
        padding: 20px 22px;
        font-family: var(--font-display);
        font-size: 14px;
        line-height: 1.75;
        border: 1px solid var(--line);
        color: var(--ink-soft);
      }
      .shiny-notification {
        border-radius: var(--radius) !important;
        font-family: var(--font-sans);
        font-size: 13px;
        box-shadow: var(--shadow-lg) !important;
      }
      .compact-spacer { margin-top: 10px; }
      hr { border-color: var(--line); }

      /* Footer status block on sidebar */
      .sidebar-status {
        margin: 18px;
        padding: 14px;
        background: rgba(255,255,255,0.04);
        border-radius: var(--radius);
        font-family: var(--font-mono);
        font-size: 10.5px;
        color: rgba(255,255,255,0.55);
        line-height: 1.6;
      }
      .sidebar-status b {
        color: rgba(255,255,255,0.85);
        font-family: var(--font-sans);
        font-weight: 600;
        font-size: 10.5px;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        display: block;
        margin-bottom: 6px;
      }

      /* Pretty switch (shinyWidgets) — afinar contraste */
      .bootstrap-switch.bootstrap-switch-on .bootstrap-switch-handle-on { background: var(--ink) !important; }
      .pretty .state label::before { border-color: var(--line) !important; }

      /* ═══ LANGUAGE SWITCHER ═══════════════════════════════════ */
      .lira-lang-switch {
        display: inline-flex;
        gap: 2px;
        background: rgba(255,255,255,0.06);
        border: 1px solid rgba(255,255,255,0.10);
        border-radius: 999px;
        padding: 3px;
        margin: 17px 4px 17px 16px;
        height: 30px;
        line-height: 1;
        align-items: center;
      }
      .lira-lang-btn {
        font-family: var(--font-mono);
        font-size: 10.5px;
        font-weight: 500;
        letter-spacing: 0.06em;
        color: rgba(255,255,255,0.55);
        background: transparent;
        border: 0;
        padding: 5px 11px;
        border-radius: 999px;
        cursor: pointer;
        transition: all 0.15s ease;
        text-transform: uppercase;
      }
      .lira-lang-btn:hover { color: rgba(255,255,255,0.95); }
      .lira-lang-btn.active {
        background: var(--accent);
        color: white;
        box-shadow: 0 1px 4px rgba(194,65,12,0.4);
      }

      /* Responsividade do header em telas pequenas */
      @media (max-width: 1100px) {
        .lira-tag { display: none; }
        .main-header .logo { width: 220px !important; }
        .main-header .navbar { margin-left: 220px !important; }
      }
      "))
    ),

    # ═══════════════════════════════════════════════════════════════
    # i18n (Português ↔ English)
    # Sistema cliente: dicionário JS + atributos data-i18n nos elementos.
    # Para estender: adicione chaves ao objeto `dict` e marque elementos
    # da UI com tags$span(`data-i18n` = "chave", "Texto padrão PT")
    # ═══════════════════════════════════════════════════════════════
    tags$script(HTML("
      const LIRA_DICT = {
        pt: {
          'header.action.new':       'Nova Análise',
          'menu.bases':              '1. Tema e Bases',
          'menu.criteria':           '2. Critérios',
          'menu.run':                '3. Executar',
          'menu.dashboard':          '4. Dashboard',
          'menu.results':            '5. Resultados',
          'menu.queue':              '6. Fila semanal',
          'menu.articles':           '7. Artigos',
          'menu.export':             '8. Exportar',
          'menu.methodology':        '9. Metodologia',
          'status.label':            'Status',
          'box.params':              'Parâmetros gerais',
          'box.sources':             'Selecionar bases de dados',
          'box.config':              'Configurações da análise',
          'box.execute':             'Executar pipeline completo',
          'chip.config':             'Configuração',
          'chip.optional':           'Opcional',
          'chip.classification':     'Classificação',
          'chip.reproducibility':    'Reprodutibilidade',
          'title.search_range':      'Intervalo e data da busca',
          'title.review_theme':      'Definir tema da revisão',
          'title.thresholds':        'Defina os cortes de P1, P2, P3 e remoção',
          'title.seed':              'Seed (Louvain)',
          'note.params_use':         'Esses parâmetros são usados na análise, nos gráficos e no texto metodológico.',
          'note.optional_section':   'Essa seção é opcional. Ative o preenchimento apenas se quiser personalizar a metodologia e identificar formalmente o projeto.',
          'note.sources_pick':       'Selecione os arquivos exportados. Qualquer combinação de bases é aceita.',
          'note.seed_explain':       'O algoritmo de Louvain tem componente estocástico. Fixar a semente garante que o mesmo resultado seja reproduzido ao re-executar o pipeline. Registre este valor na seção de métodos do manuscrito.',
          'switch.enable_theme':     'Habilitar preenchimento do tema',
          'label.search_date':       'Data da busca',
          'label.start_year':        'Ano inicial',
          'label.no_file':           'Nenhum arquivo selecionado'
        },
        en: {
          'header.action.new':       'New Analysis',
          'menu.bases':              '1. Topic & Sources',
          'menu.criteria':           '2. Criteria',
          'menu.run':                '3. Run',
          'menu.dashboard':          '4. Dashboard',
          'menu.results':            '5. Results',
          'menu.queue':              '6. Reading queue',
          'menu.articles':           '7. Articles',
          'menu.export':             '8. Export',
          'menu.methodology':        '9. Methodology',
          'status.label':            'Status',
          'box.params':              'General parameters',
          'box.sources':             'Select databases',
          'box.config':              'Analysis settings',
          'box.execute':             'Run complete pipeline',
          'chip.config':             'Configuration',
          'chip.optional':           'Optional',
          'chip.classification':     'Classification',
          'chip.reproducibility':    'Reproducibility',
          'title.search_range':      'Search range and date',
          'title.review_theme':      'Define review topic',
          'title.thresholds':        'Set cutoffs for P1, P2, P3 and removal',
          'title.seed':              'Seed (Louvain)',
          'note.params_use':         'These parameters are used in the analysis, charts and methods text.',
          'note.optional_section':   'This section is optional. Enable filling only if you want to customize the methodology and formally identify the project.',
          'note.sources_pick':       'Select the exported files. Any combination of databases is accepted.',
          'note.seed_explain':       'The Louvain algorithm has a stochastic component. Fixing the seed guarantees the same result when re-running the pipeline. Record this value in the methods section of the manuscript.',
          'switch.enable_theme':     'Enable topic filling',
          'label.search_date':       'Search date',
          'label.start_year':        'Start year',
          'label.no_file':           'No file selected'
        }
      };

      function liraSetLang(lang) {
        if (!LIRA_DICT[lang]) return;
        document.querySelectorAll('[data-i18n]').forEach(function(el) {
          var key = el.getAttribute('data-i18n');
          var t = LIRA_DICT[lang][key];
          if (t !== undefined) el.textContent = t;
        });
        document.querySelectorAll('.lira-lang-btn').forEach(function(b) {
          b.classList.toggle('active', b.getAttribute('data-lang') === lang);
        });
        try { localStorage.setItem('lira-lang', lang); } catch(e) {}
        if (window.Shiny) Shiny.setInputValue('lira_lang', lang, {priority: 'event'});
      }

      document.addEventListener('DOMContentLoaded', function() {
        var saved = 'pt';
        try { saved = localStorage.getItem('lira-lang') || 'pt'; } catch(e) {}
        // Atraso pequeno para garantir que Shiny renderizou a UI
        setTimeout(function() { liraSetLang(saved); }, 200);
      });

      // Observa novos elementos inseridos dinamicamente (ex.: renderUI)
      // e aplica a tradução atual a eles
      var liraObserver = new MutationObserver(function(mutations) {
        var lang = 'pt';
        try { lang = localStorage.getItem('lira-lang') || 'pt'; } catch(e) {}
        mutations.forEach(function(m) {
          m.addedNodes.forEach(function(n) {
            if (n.nodeType !== 1) return;
            if (n.hasAttribute && n.hasAttribute('data-i18n')) {
              var k = n.getAttribute('data-i18n');
              if (LIRA_DICT[lang][k]) n.textContent = LIRA_DICT[lang][k];
            }
            if (n.querySelectorAll) {
              n.querySelectorAll('[data-i18n]').forEach(function(el) {
                var k = el.getAttribute('data-i18n');
                if (LIRA_DICT[lang][k]) el.textContent = LIRA_DICT[lang][k];
              });
            }
          });
        });
      });
      document.addEventListener('DOMContentLoaded', function() {
        liraObserver.observe(document.body, {childList: true, subtree: true});
      });
    ")),

    tabItems(

      # ════════════════════════════════════════════════════════
      # ABA 1 — TEMA E BASES
      # ════════════════════════════════════════════════════════
      tabItem(
        "bases",
        fluidRow(
          box(
            title = tags$span(`data-i18n` = "box.params", "Parâmetros gerais"),
            width = 12, status = "primary", solidHeader = TRUE,
            div(
              class = "clean-card soft",
              fluidRow(
                column(
                  6,
                  div(class = "section-chip", `data-i18n` = "chip.config", "Configuração"),
                  div(class = "sec-title", `data-i18n` = "title.search_range", "Intervalo e data da busca"),
                  p(class = "minor-note", `data-i18n` = "note.params_use", "Esses parâmetros são usados na análise, nos gráficos e no texto metodológico."),
                  fluidRow(
                    column(6, dateInput("data_busca", "Data da busca", value = Sys.Date(), language = "pt")),
                    column(6, numericInput("ano_inicial", "Ano inicial", value = 2015, min = 1990, max = 2035))
                  )
                ),
                column(
                  6,
                  div(class = "section-chip", `data-i18n` = "chip.optional", "Opcional"),
                  div(class = "sec-title", `data-i18n` = "title.review_theme", "Definir tema da revisão"),
                  prettySwitch("habilitar_tema", "Habilitar preenchimento do tema", value = FALSE, status = "primary", fill = TRUE),
                  conditionalPanel(
                    condition = "input.habilitar_tema == true",
                    div(
                      class = "compact-spacer",
                      fluidRow(
                        column(8,
                               textInput("tema_titulo", "Título do tema", placeholder = "Ex: CRISPR-Cas9 in cancer therapy"),
                               textAreaInput("tema_descricao", "Descrição do tema (opcional)", placeholder = "Descreva o foco do review...", rows = 2, width = "100%")
                        ),
                        column(4,
                               textInput("autor_nome", "Autor principal", placeholder = "Ex: Basílio LA"),
                               textInput("instituicao", "Instituição", placeholder = "Ex: UFES, Vitória, ES, Brasil")
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.habilitar_tema == false",
                    div(class = "disabled-note", `data-i18n` = "note.optional_section", "Essa seção é opcional. Ative o preenchimento apenas se quiser personalizar a metodologia e identificar formalmente o projeto.")
                  )
                )
              )
            )
          )
        ),

        fluidRow(
          box(
            title = tags$span(`data-i18n` = "box.sources", "Selecionar bases de dados"),
            width = 12, status = "primary", solidHeader = TRUE,
            tags$p(
              `data-i18n` = "note.sources_pick",
              style = "color:#475569; font-size:13px; margin-bottom:12px;",
              "Selecione os arquivos exportados. Qualquer combinação de bases é aceita."
            ),

            fluidRow(
              column(
                4,
                tags$div(
                  class = "db-card",
                  tags$div(
                    style = "display:flex; align-items:center; margin-bottom:8px;",
                    icon("book-medical", style = "color:#B91C1C; font-size:22px; margin-right:10px;"),
                    tags$div(
                      tags$b("PubMed / MEDLINE"),
                      tags$div(style = "font-size:11px; color:#94A3B8;", "Formato: .txt ou .nbib")
                    )
                  ),
                  shinyFilesButton(
                    "file_pubmed", "Selecionar arquivo", "Arquivo PubMed (.txt/.nbib)",
                    multiple = FALSE,
                    style = "width:100%; background:#B91C1C; color:white; border:none; border-radius:8px; padding:8px;"
                  ),
                  tags$br(), tags$br(),
                  uiOutput("status_pubmed")
                )
              ),
              column(
                4,
                tags$div(
                  class = "db-card",
                  tags$div(
                    style = "display:flex; align-items:center; margin-bottom:8px;",
                    icon("flask", style = "color:#1E40AF; font-size:22px; margin-right:10px;"),
                    tags$div(
                      tags$b("Web of Science"),
                      tags$div(style = "font-size:11px; color:#94A3B8;", "Formato: .txt (Plain Text)")
                    )
                  ),
                  shinyFilesButton(
                    "file_wos", "Selecionar arquivo", "Arquivo WoS (.txt)",
                    multiple = FALSE,
                    style = "width:100%; background:#1E40AF; color:white; border:none; border-radius:8px; padding:8px;"
                  ),
                  tags$br(), tags$br(),
                  uiOutput("status_wos")
                )
              ),
              column(
                4,
                tags$div(
                  class = "db-card",
                  tags$div(
                    style = "display:flex; align-items:center; margin-bottom:8px;",
                    icon("graduation-cap", style = "color:#C2410C; font-size:22px; margin-right:10px;"),
                    tags$div(
                      tags$b("Scopus"),
                      tags$div(style = "font-size:11px; color:#94A3B8;", "Formato: .bib, .ris ou .csv")
                    )
                  ),
                  shinyFilesButton(
                    "file_scopus", "Selecionar arquivo", "Arquivo Scopus",
                    multiple = FALSE,
                    style = "width:100%; background:#C2410C; color:white; border:none; border-radius:8px; padding:8px;"
                  ),
                  tags$br(),
                  radioGroupButtons(
                    "scopus_format", label = NULL,
                    choices = c("BibTeX" = "bibtex", "RIS" = "ris", "CSV" = "csv"),
                    selected = "bibtex", size = "sm", status = "warning"
                  ),
                  uiOutput("status_scopus")
                )
              )
            ),

            tags$hr(),
            div(
              class = "clean-card soft",
              div(class = "section-chip", "Opcional"),
              div(class = "sec-title", "Adicionar outra base"),
              prettySwitch("usar_base_extra", "Habilitar base adicional", value = FALSE, status = "success", fill = TRUE),
              conditionalPanel(
                condition = "input.usar_base_extra == true",
                div(
                  class = "compact-spacer",
                  fluidRow(
                    column(3, textInput("extra_dbname", "Nome", placeholder = "Ex: LILACS")),
                    column(3, selectInput("extra_dbsource", "Tipo", choices = c("pubmed","wos","scopus","isi","cochrane"), selected = "pubmed")),
                    column(3, selectInput("extra_format", "Formato", choices = c("pubmed","plaintext","bibtex","ris","csv"), selected = "pubmed")),
                    column(
                      3, tags$br(),
                      shinyFilesButton(
                        "file_extra", "Selecionar arquivo", "Arquivo extra",
                        multiple = FALSE,
                        style = "width:100%; background:#047857; color:white; border:none; border-radius:8px; padding:8px;"
                      )
                    )
                  ),
                  uiOutput("status_extra")
                )
              ),
              conditionalPanel(
                condition = "input.usar_base_extra == false",
                div(class = "disabled-note", "Desabilitado por padrão. Ative apenas se quiser incluir uma base adicional além de PubMed, WoS e Scopus.")
              )
            ),
            tags$hr(),
            tags$div(
              style = "text-align:right;",
              actionBttn(
                "btn_carregar", "Carregar e Mesclar Bases",
                style = "fill", color = "primary",
                icon = icon("layer-group"), size = "md"
              )
            )
          )
        ),
        fluidRow(uiOutput("corpus_summary_boxes"))
      ),

      # ════════════════════════════════════════════════════════
      # ABA 2 — CRITÉRIOS
      # ════════════════════════════════════════════════════════
      tabItem(
        "criterios",
        fluidRow(
          box(
            title = "Configurar critérios de ranking",
            width = 12, status = "primary", solidHeader = TRUE,
            tags$p(
              style = "color:#475569; font-size:13px;",
              "Defina aqui como o ranking será construído: eixo temático, tratamento de citações, uso de revistas e critérios temáticos."
            ),

            fluidRow(
              column(
                6,
                div(
                  class = "subsection-card",
                  div(class = "section-chip", "1"),
                  div(class = "sec-title", "Eixo temático"),
                  radioGroupButtons(
                    "usar_eixo_tematico", label = NULL,
                    choices = c("Usar eixo temático" = "sim", "Não usar eixo temático" = "nao"),
                    selected = "sim", status = "primary", justified = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.usar_eixo_tematico == 'sim'",
                    tagList(
                      sliderInput("peso_tematico", "Peso temático (%)", min = 10, max = 80, value = 60, step = 5, width = "100%"),
                      uiOutput("peso_bib_display")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.usar_eixo_tematico == 'nao'",
                    div(class = "disabled-note", "O ranking será calculado apenas com o componente bibliométrico.")
                  )
                )
              ),
              column(
                6,
                div(
                  class = "subsection-card",
                  div(class = "section-chip", "2"),
                  div(class = "sec-title", "Tratamento de citações"),
                  radioGroupButtons(
                    "modo_citacoes_tempo", label = NULL,
                    choices = c(
                      "Sem ponderação temporal" = "bruto",
                      "Ponderar por citações/ano" = "ponderado"
                    ),
                    selected = "bruto", status = "success", justified = TRUE
                  ),
                  checkboxInput("usar_recencia", "Incluir score de recência no componente bibliométrico", value = TRUE),
                  tags$p(class = "minor-note", "A opção ponderada reduz a vantagem automática de artigos muito antigos. A recência continua sendo um componente separado, quando ativada.")
                )
              )
            ),

            fluidRow(
              column(
                6,
                div(
                  class = "subsection-card",
                  div(class = "section-chip", "3"),
                  div(class = "sec-title", "Revistas"),
                  prettySwitch("usar_score_revistas", "Usar score de revistas", value = FALSE, status = "warning", fill = TRUE),
                  conditionalPanel(
                    condition = "input.usar_score_revistas == true",
                    tagList(
                      textAreaInput("revistas_top", "Tier 1 — Alto impacto (score 5)", placeholder = "Ex: NATURE|SCIENCE|CELL|NUCLEIC ACIDS|NAT METHODS", rows = 2, width = "100%"),
                      textAreaInput("revistas_med", "Tier 2 — Impacto médio (score 4)", placeholder = "Ex: NAT REV|TRENDS|CURR OPIN|BIOINFORMATICS|NAT COMMUN", rows = 2, width = "100%"),
                      textAreaInput("revistas_tier3", "Tier 3 — Impacto moderado (score 3)",
                                    value = "FRONT|BMC|SCI REP|MARINE DRUGS|J ETHNOPHARMACOL|PHYTOMEDICINE",
                                    rows = 2, width = "100%"),
                      textAreaInput("revistas_tier4", "Tier 4 — Impacto básico (score 2)",
                                    value = "INT J MOL SCI|MOLECULES|METABOLITES",
                                    rows = 2, width = "100%"),
                      tags$p(class = "minor-note", "Revistas não enquadradas em nenhum tier recebem score 1. Use | como separador. Edite os tiers 3 e 4 conforme sua área de pesquisa.")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.usar_score_revistas == false",
                    div(class = "disabled-note", "Desabilitado por padrão. Ative apenas se quiser que o periódico influencie explicitamente o score bibliométrico.")
                  )
                )
              ),
              column(
                6,
                div(
                  class = "subsection-card",
                  div(class = "section-chip", "4"),
                  div(class = "sec-title", "Critérios temáticos"),
                  conditionalPanel(
                    condition = "input.usar_eixo_tematico == 'sim'",
                    tagList(
                      textAreaInput(
                        "string_busca", "String de busca",
                        placeholder = 'Ex: ("CRISPR" OR "Cas9") AND ("cancer" OR "tumor") AND ("delivery" OR "nanoparticles")',
                        rows = 4, width = "100%"
                      ),
                      radioGroupButtons(
                        "modo_eixo_tematico", label = "Como preencher o eixo temático",
                        choices = c("Manual" = "manual", "Gerar pela string de busca" = "busca", "String + manual" = "hibrido"),
                        selected = "hibrido", status = "info", justified = TRUE
                      ),
                      tags$p(class = "minor-note", "Você pode construir os critérios do zero, gerar pela string ou combinar as duas abordagens.")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.usar_eixo_tematico == 'nao'",
                    div(class = "disabled-note", "O eixo temático está desativado. Ative-o para usar critérios manuais ou gerar critérios pela string de busca.")
                  )
                )
              )
            ),

            conditionalPanel(
              condition = "input.usar_eixo_tematico == 'sim'",
              div(
                class = "subsection-card",
                fluidRow(
                  column(
                    12,
                    div(class = "section-chip", "Tabela temática"),
                    div(class = "sec-title", "Tabela de critérios"),
                    conditionalPanel(
                      condition = "input.modo_eixo_tematico != 'manual'",
                      div(
                        class = "clean-card soft",
                        tags$b("Gerar critérios a partir da string de busca"),
                        tags$br(), tags$br(),
                        fluidRow(
                          column(4, actionBttn("btn_substituir_crit_busca", "Substituir pela string", style = "fill", color = "primary", icon = icon("wand-magic-sparkles"), size = "sm")),
                          column(4, actionBttn("btn_somar_crit_busca", "Somar à tabela atual", style = "fill", color = "warning", icon = icon("plus"), size = "sm")),
                          column(4, actionBttn("btn_limpar_criterios", "Limpar critérios", style = "fill", color = "danger", icon = icon("trash"), size = "sm"))
                        ),
                        tags$p(class = "minor-note", "Use a string para sugerir grupos temáticos e depois refine manualmente, se quiser.")
                      )
                    ),
                    DTOutput("tabela_criterios"),
                    tags$br(),
                    conditionalPanel(
                      condition = "input.modo_eixo_tematico != 'busca'",
                      div(
                        class = "clean-card soft",
                        tags$b("Adicionar critério manualmente"),
                        tags$br(), tags$br(),
                        fluidRow(
                          column(4, textInput("novo_nome", "Nome do critério", placeholder = "Ex: Biodiversidade tropical")),
                          column(1, numericInput("novo_peso", "Peso", value = 3, min = 1, max = 5)),
                          column(5, textAreaInput("novo_kw", "Palavras-chave (separadas por ;)", placeholder = "Ex: ATLANTIC FOREST; TROPICAL; BIODIVERSITY; CERRADO", rows = 2)),
                          column(2, tags$br(), actionBttn("btn_add_crit", "Adicionar", style = "fill", color = "success", icon = icon("plus"), size = "sm"))
                        ),
                        tags$p(class = "minor-note", "As palavras-chave são buscadas em TÍTULO + ABSTRACT + keywords do artigo.")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════════════
      # ABA 3 — EXECUTAR
      # ════════════════════════════════════════════════════════
      tabItem(
        "pipeline",
        fluidRow(
          box(
            title = tags$span(`data-i18n` = "box.config", "Configurações da análise"),
            width = 12, status = "warning", solidHeader = TRUE,
            div(
              class = "clean-card soft",
              div(class = "section-chip", `data-i18n` = "chip.classification", "Classificação"),
              div(class = "sec-title", `data-i18n` = "title.thresholds", "Defina os cortes de P1, P2, P3 e remoção"),
              fluidRow(
                column(3, numericInput("limiar_p1", "P1 (score ≥)", value = 55, min = 1, max = 100)),
                column(3, numericInput("limiar_p2", "P2 (score ≥)", value = 40, min = 1, max = 100)),
                column(3, numericInput("limiar_p3", "P3 (score ≥)", value = 25, min = 1, max = 100)),
                column(3, numericInput("limiar_removido", "Removidos (score <)", value = 10, min = 0, max = 100))
              ),
              uiOutput("status_limiares")
            ),
            div(
              class = "clean-card soft",
              style = "margin-top: 14px;",
              fluidRow(
                column(3,
                  div(class = "section-chip", style = "margin-bottom: 8px;", `data-i18n` = "chip.reproducibility", "Reprodutibilidade"),
                  div(style = "font-family: var(--font-display); font-size: 14px; font-weight: 500; color: var(--ink); margin-bottom: 8px;", `data-i18n` = "title.seed", "Seed (Louvain)"),
                  numericInput("louvain_seed", label = NULL, value = 42L, min = 1L, max = 99999L, step = 1L, width = "100%")
                ),
                column(9,
                  tags$p(
                    class = "minor-note",
                    `data-i18n` = "note.seed_explain",
                    style = "margin-top: 32px; padding-left: 12px; border-left: 2px solid var(--line); line-height: 1.6;",
                    "O algoritmo de Louvain tem componente estocástico. Fixar a semente garante que o mesmo resultado seja reproduzido ao re-executar o pipeline. Registre este valor na seção de métodos do manuscrito."
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Executar pipeline completo",
            width = 12, status = "success", solidHeader = TRUE,
            tags$div(
              style = "text-align:center; padding:20px;",
              tags$h4(style = "color:#0B1F3F;", "Pipeline de análise"),
              tags$p(style = "color:#666;", "Carregue as bases, configure os critérios e execute a análise completa."),
              tags$br(),
              fluidRow(
                column(3, tags$div(class = "criteria-card", icon("database", style = "color:#1E40AF; font-size:20px;"), tags$b(" Passo 1"), tags$br(), tags$small("Importar e mesclar bases"))),
                column(3, tags$div(class = "criteria-card", icon("chart-line", style = "color:#1E8449; font-size:20px;"), tags$b(" Passo 2"), tags$br(), tags$small("Análise bibliométrica"))),
                column(3, tags$div(class = "criteria-card", icon("sliders", style = "color:#C2410C; font-size:20px;"), tags$b(" Passo 3"), tags$br(), tags$small("Aplicar cortes e classes"))),
                column(3, tags$div(class = "criteria-card", icon("file-excel", style = "color:#047857; font-size:20px;"), tags$b(" Passo 4"), tags$br(), tags$small("Gerar Excel + figuras")))
              ),
              tags$br(),
              actionBttn(
                "btn_executar", "Executar Pipeline Completo",
                style = "fill", color = "success", size = "lg",
                icon = icon("rocket")
              )
            ),
            tags$hr(),
            tags$div(class = "sec-title", "Log de execução"),
            verbatimTextOutput("log_output", placeholder = TRUE)
          )
        )
      ),

      # ════════════════════════════════════════════════════════
      # ABA 4 — DASHBOARD
      # ════════════════════════════════════════════════════════
      tabItem(
        "dashboard",
        fluidRow(
          box(
            title = "Dashboard executivo",
            width = 12, status = "primary", solidHeader = TRUE,
            uiOutput("dashboard_header")
          )
        ),
        fluidRow(uiOutput("dashboard_priority_boxes")),
        fluidRow(
          box(
            title = "Indicadores bibliométricos do corpus",
            width = 7, status = "primary",
            DTOutput("dashboard_table")
          ),
          box(
            title = "Distribuição das classes",
            width = 5, status = "info",
            plotOutput("plot_class_dist", height = "340px")
          )
        ),
        fluidRow(
          box(
            title = "Produção anual e tendência",
            width = 7, status = "primary",
            plotOutput("dash_plot_annual", height = "340px")
          ),
          box(
            title = "Distribuição do score final",
            width = 5, status = "warning",
            plotOutput("dash_plot_score_dist", height = "340px")
          )
        ),
        fluidRow(
          box(
            title = "Top artigos P1",
            width = 12, status = "success",
            DTOutput("table_dashboard_top")
          )
        )
      ),

      # ════════════════════════════════════════════════════════
      # ABA 5 — RESULTADOS
      # ════════════════════════════════════════════════════════
      tabItem(
        "resultados",
        fluidRow(uiOutput("result_info_boxes")),
        fluidRow(
          tabBox(
            title = "Análise Bibliométrica", width = 12,
            tabPanel(
              "Produção Anual",
              plotOutput("plot_annual", height = "380px"),
              tags$div(
                style = "background:#EBF5FB; padding:12px; border-radius:8px; margin-top:8px; font-size:12px;",
                tags$b("Interpretação: "), textOutput("interp_annual", inline = TRUE)
              )
            ),
            tabPanel(
              "Top Revistas",
              plotOutput("plot_journals", height = "420px"),
              tags$div(
                style = "background:#EBF5FB; padding:12px; border-radius:8px; margin-top:8px; font-size:12px;",
                tags$b("Interpretação: "), textOutput("interp_journals", inline = TRUE)
              )
            ),
            tabPanel(
              "Top Países",
              plotOutput("plot_countries", height = "400px"),
              tags$div(
                style = "background:#D5F5E3; padding:12px; border-radius:8px; margin-top:8px; font-size:12px;",
                tags$b("Destaque: "), textOutput("interp_countries", inline = TRUE)
              )
            ),
            tabPanel("Top Autores", plotOutput("plot_authors", height = "400px")),
            tabPanel(
              "Top Keywords",
              plotOutput("plot_keywords", height = "420px"),
              tags$div(
                style = "background:#EBF5FB; padding:12px; border-radius:8px; margin-top:8px; font-size:12px;",
                tags$b("Interpretação: "), textOutput("interp_keywords", inline = TRUE)
              )
            ),
            tabPanel("Top Instituições", plotOutput("plot_institutions", height = "420px")),
            tabPanel("Distribuição de citações", plotOutput("plot_citations_dist", height = "420px")),
            tabPanel("Distribuição do score", plotOutput("plot_score_dist", height = "420px")),
            tabPanel("Score temático × bibliométrico", plotOutput("plot_score_scatter", height = "420px")),
            tabPanel("Rede de keywords", plotOutput("plot_kw_network", height = "520px")),
            tabPanel("Rede de coautoria", plotOutput("plot_author_network", height = "520px")),
            tabPanel("Rede entre países", plotOutput("plot_country_network", height = "520px")),
            tabPanel("Métricas de rede", DTOutput("table_network_metrics")),
            tabPanel("Clusters de keywords", DTOutput("table_kw_clusters")),
            tabPanel("Mais Citados", DTOutput("table_cited"))
          )
        )
      ),

      # ════════════════════════════════════════════════════════
      # ABA 6 — FILA SEMANAL
      # ════════════════════════════════════════════════════════
      tabItem(
        "fila_semanal",
        fluidRow(
          box(
            title = "Fila semanal sugerida",
            width = 12, status = "primary", solidHeader = TRUE,
            tags$p(style = "color:#475569; font-size:13px;",
                   "Sequência automática para leitura em seis blocos, com destaque explícito para Artigos fundadores, impacto, atualidade e síntese."),
            fluidRow(
              column(
                4,
                radioGroupButtons(
                  "filter_semana",
                  label = "Semana",
                  choices = c(
                    "Todas" = "TODAS",
                    "Semana 1" = "Semana 1 - Artigos fundadores",
                    "Semana 2" = "Semana 2 - Obrigatórios recentes",
                    "Semana 3" = "Semana 3 - Recomendados de maior impacto",
                    "Semana 4" = "Semana 4 - Recomendados complementares",
                    "Semana 5" = "Semana 5 - Opcionais estratégicos",
                    "Semana 6" = "Semana 6 - Reviews e sínteses"
                  ),
                  selected = "TODAS", status = "primary", justified = TRUE
                )
              ),
              column(
                8,
                div(class = "minor-note",
                    "A fila considera apenas P1, P2 e P3. Artigos fundadores são identificados por alto impacto; reviews e revisões sistemáticas entram na semana 6; removidos não entram na fila.")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = uiOutput("fila_title"),
            width = 12, status = "success",
            DTOutput("table_fila")
          )
        )
      ),

      # ════════════════════════════════════════════════════════
      # ABA 7 — ARTIGOS
      # ════════════════════════════════════════════════════════
      tabItem(
        "artigos",
        fluidRow(
          box(
            title = "Filtros",
            width = 12, collapsible = TRUE, collapsed = FALSE,
            fluidRow(
              column(
                4,
                radioGroupButtons(
                  "filter_prio", label = "Classe",
                  choices = c(
                    "Todos" = "TODOS",
                    "P1" = "P1 - OBRIGATORIA",
                    "P2" = "P2 - RECOMENDADA",
                    "P3" = "P3 - OPCIONAL",
                    "Abaixo do corte" = "ABAIXO DO CORTE",
                    "P4 - Removidos" = "P4 - REMOVIDO"
                  ),
                  selected = "TODOS", status = "primary", justified = TRUE
                )
              ),
              column(3, sliderInput("filter_score", "Score mínimo", min = 0, max = 100, value = 0, step = 1)),
              column(2, numericInput("filter_cit", "Citações mínimas", value = 0, min = 0)),
              column(3, textInput("filter_txt", "Buscar no título/keywords", placeholder = "Ex: marine, CRISPR, Atlantic..."))
            )
          )
        ),
        fluidRow(
          box(
            title = uiOutput("art_title"),
            width = 12, status = "primary",
            DTOutput("table_artigos")
          )
        )
      ),

      # ════════════════════════════════════════════════════════
      # ABA 8 — EXPORTAR
      # ════════════════════════════════════════════════════════
      tabItem(
        "exportar",
        fluidRow(
          box(
            title = "Exportar Excel estilizado",
            width = 8, status = "success", solidHeader = TRUE,
            tags$div(class = "sec-title", "Selecione as abas:"),
            checkboxGroupInput(
              "export_sheets", label = NULL,
              choices = c(
                "Dashboard com indicadores" = "dashboard",
                "P1 - Leitura Obrigatoria" = "p1",
                "P2 - Leitura Recomendada" = "p2",
                "P3 - Opcional" = "p3",
                "Fila semanal" = "fila",
                "Bibliometria robusta" = "bibliometria",
                "Abaixo do corte" = "abaixo",
                "P4 - Removidos" = "p4",
                "PRISMA (numeros para manuscrito)" = "prisma",
                "Template de Anotacao" = "template",
                "Ranking Completo" = "ranking",
                "Configuracoes da Analise" = "config"
              ),
              selected = c("dashboard","p1","p2","p3","fila","bibliometria","ranking","prisma","template","config"),
              inline = FALSE
            ),
            tags$hr(),
            textInput("export_nome", "Nome do arquivo", value = paste0("review_bibliometria_", format(Sys.Date(), "%Y%m%d"))),
            tags$br(),
            downloadBttn(
              "btn_download", "Baixar Excel Estilizado (.xlsx)",
              style = "fill", color = "success", size = "lg",
              icon = icon("download"), block = TRUE
            )
          ),
          box(
            title = "Exportar figuras (PNG 300 DPI)",
            width = 4, status = "info",
            tags$p("Figuras geradas pelo pipeline:"), tags$br(),
            downloadButton("dl_fig1", "Fig 1 — Produção Anual", style = "width:100%; margin-bottom:8px; background:#1E40AF; color:white;"),
            downloadButton("dl_fig2", "Fig 2 — Top Revistas", style = "width:100%; margin-bottom:8px; background:#E74C3C; color:white;"),
            downloadButton("dl_fig3", "Fig 3 — Top Países", style = "width:100%; margin-bottom:8px; background:#1E8449; color:white;"),
            downloadButton("dl_fig4", "Fig 4 — Top Autores", style = "width:100%; margin-bottom:8px; background:#1ABC9C; color:white;"),
            downloadButton("dl_fig5", "Fig 5 — Top Keywords", style = "width:100%; background:#9B59B6; color:white;")
          )
        )
      ),

      # ════════════════════════════════════════════════════════
      # ABA 9 — METODOLOGIA
      # ════════════════════════════════════════════════════════
      tabItem(
        "metodologia",
        fluidRow(
          box(
            title = "Texto de Metodologia para o Manuscrito",
            width = 12, status = "primary", solidHeader = TRUE,
            tags$p(
              style = "color:#475569; font-size:13px;",
              "Texto gerado automaticamente com base nas suas configurações."
            ),
            fluidRow(
              column(
                4,
                selectInput("metod_idioma", "Idioma", choices = c("Português" = "pt", "English" = "en"), selected = "pt"),
                tags$br(),
                actionBttn("btn_gerar_metod", "Gerar Texto", style = "fill", color = "primary", icon = icon("magic"), size = "md"),
                tags$br(), tags$br(),
                downloadButton("dl_metodologia", "Baixar como .txt", style = "width:100%; background:#1E40AF; color:white;")
              ),
              column(
                8,
                tags$div(class = "metod-box", uiOutput("texto_metodologia"))
              )
            )
          )
        )
      )
    )
  )
)

# ================================================================
# SERVER
# ================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(
    df_merged = NULL,
    results = NULL,
    df_score = NULL,
    criterios = default_criteria,
    plots = list(),
    log = character(0),
    n_pubmed = 0L,
    n_wos = 0L,
    n_scopus = 0L,
    n_extra = 0L,
    dup_rem = 0L,
    texto_metod = "",
    net_kw = NULL,
    net_au = NULL,
    net_co = NULL,
    py_median = NA_real_,
    n_sem_ano = 0L
  )

  log_add <- function(msg) {
    rv$log <- c(rv$log, paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg))
  }

  # ── Nova análise ─────────────────────────────────────────────
  observeEvent(input$btn_nova_analise, {
    showModal(modalDialog(
      title = "Iniciar nova análise?",
      tags$p("Todos os dados, arquivos carregados e resultados serão apagados."),
      tags$p(style = "color:#B91C1C; font-weight:bold;", "Esta ação não pode ser desfeita."),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_confirma_reset", "Sim, nova análise", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$btn_confirma_reset, {
    removeModal()
    session$reload()
  })

  # ── File choosers ────────────────────────────────────────────
  # getVolumes() detecta automaticamente todas as unidades disponíveis:
  #   Windows: C:, D:, E:, ... + atalhos (Documents, Downloads, Desktop)
  #   macOS:   / (raiz), /Volumes/* (discos externos), ~ (home)
  #   Linux:   /, /home, /media, /mnt
  # Inclui também Home, Downloads, Desktop e Documents como atalhos nomeados.
  roots <- c(
    Home      = path.expand("~"),
    Documents = path.expand("~/Documents"),
    Downloads = path.expand("~/Downloads"),
    Desktop   = path.expand("~/Desktop"),
    shinyFiles::getVolumes()()
  )

  shinyFileChoose(input, "file_pubmed", roots = roots, filetypes = c("txt", "nbib"))
  shinyFileChoose(input, "file_wos",    roots = roots, filetypes = c("txt"))
  shinyFileChoose(input, "file_scopus", roots = roots, filetypes = c("bib", "ris", "csv"))
  shinyFileChoose(input, "file_extra",  roots = roots)

  get_path <- function(fi) {
    if (is.null(fi) || is.integer(fi)) return(NULL)
    tryCatch(parseFilePaths(roots, fi)$datapath[1], error = function(e) NULL)
  }

  st_ui <- function(p) {
    if (is.null(p)) return(tags$small(style = "color:#94A3B8;", "Nenhum arquivo selecionado"))
    tags$div(
      icon("check-circle", style = "color:#1E8449;"),
      tags$small(style = "color:#1E8449;", basename(p))
    )
  }

  output$status_pubmed <- renderUI(st_ui(get_path(input$file_pubmed)))
  output$status_wos    <- renderUI(st_ui(get_path(input$file_wos)))
  output$status_scopus <- renderUI(st_ui(get_path(input$file_scopus)))
  output$status_extra  <- renderUI({
    if (!isTRUE(input$usar_base_extra)) return(NULL)
    p <- get_path(input$file_extra)
    if (is.null(p)) return(tags$small(style = "color:#94A3B8;", "Nenhum arquivo selecionado"))
    tags$div(
      icon("check-circle", style = "color:#1E8449;"),
      tags$small(style = "color:#1E8449;", paste0(input$extra_dbname, ": ", basename(p)))
    )
  })

  # ── Exibição pesos ───────────────────────────────────────────
  output$peso_bib_display <- renderUI({
    usar_eixo <- identical(input$usar_eixo_tematico, "sim")
    peso_t <- if (usar_eixo) input$peso_tematico else 0
    peso_b <- 100 - peso_t

    tags$div(
      tags$b("Score bibliométrico: "),
      tags$span(paste0(peso_b, "%"), style = "color:#1E8449; font-weight:bold;"),
      tags$br(),
      if (usar_eixo) tags$small(
        class = "minor-note",
        "Componente temático + componente bibliométrico"
      ) else tags$small(
        class = "minor-note",
        "Modo somente bibliométrico"
      )
    )
  })

  output$status_limiares <- renderUI({
    if (thresholds_are_valid(input$limiar_p1, input$limiar_p2, input$limiar_p3, input$limiar_removido)) {
      div(class = "minor-note", HTML("Ordem válida: <b>P1 &gt; P2 &gt; P3 &gt; Removidos</b>. Artigos abaixo do corte de remoção serão classificados como P4 - Removido; artigos entre esse corte e P3 ficam em 'Abaixo do corte'."))
    } else {
      div(style = "color:#B91C1C; font-weight:600; font-size:12px;", "Ajuste os limiares para que P1 > P2 > P3 > Removidos ≥ 0.")
    }
  })

  classified_df <- reactive({
    req(rv$df_score)
    validate(need(thresholds_are_valid(input$limiar_p1, input$limiar_p2, input$limiar_p3, input$limiar_removido),
                  "Os limiares devem obedecer a ordem P1 > P2 > P3 > Removidos ≥ 0."))
    apply_priority_labels(rv$df_score, input$limiar_p1, input$limiar_p2, input$limiar_p3, input$limiar_removido)
  })


  dashboard_indicators <- reactive({
    df <- classified_df()
    annual_df <- rv$df_merged %>%
      mutate(Year = suppressWarnings(as.numeric(PY))) %>%
      filter(!is.na(Year), Year >= input$ano_inicial) %>%
      count(Year, name = "N") %>%
      arrange(Year)

    cagr <- calc_growth_rate(annual_df$N)

    top_country <- tryCatch({
      co <- get_country_col(rv$df_merged)
      if (length(co) == 0 || is.na(co)) return(NA_character_)
      rv$df_merged %>%
        filter(!is.na(.data[[co]]), as.character(.data[[co]]) != "") %>%
        mutate(CO = strsplit(as.character(.data[[co]]), ";")) %>%
        tidyr::unnest(CO) %>%
        mutate(CO = trimws(toupper(CO))) %>%
        filter(CO != "", nchar(CO) <= 30) %>%
        count(CO, sort = TRUE) %>%
        slice_head(n = 1) %>%
        pull(CO)
    }, error = function(e) NA_character_)

    top_journal <- rv$df_merged %>%
      mutate(SO = ifelse(is.na(SO), "", SO)) %>%
      filter(SO != "") %>%
      count(SO, sort = TRUE) %>%
      slice_head(n = 1) %>%
      pull(SO)
    if (length(top_journal) == 0) top_journal <- NA_character_

    top_keyword <- rv$df_merged %>%
      mutate(DE = ifelse(is.na(DE), "", DE)) %>%
      filter(DE != "") %>%
      mutate(K = strsplit(toupper(DE), ";")) %>%
      tidyr::unnest(K) %>%
      mutate(K = trimws(K)) %>%
      filter(K != "") %>%
      count(K, sort = TRUE) %>%
      slice_head(n = 1) %>%
      pull(K)
    if (length(top_keyword) == 0) top_keyword <- NA_character_

    years <- suppressWarnings(as.numeric(rv$df_merged$PY))
    years <- years[!is.na(years)]
    period_val <- if (length(years) > 0) paste0(min(years), " - ", max(years)) else "N/D"

    bases_incl <- paste(c(
      if (rv$n_pubmed > 0) "PubMed" else NULL,
      if (rv$n_scopus > 0) "Scopus" else NULL,
      if (rv$n_wos > 0) "Web of Science" else NULL,
      if (rv$n_extra > 0) input$extra_dbname else NULL
    ), collapse = " + ")
    if (!nzchar(bases_incl)) bases_incl <- "N/D"

    data.frame(
      Indicador = c(
        "Total artigos (após deduplicação)",
        "Bases incluídas",
        "Período coberto",
        "Taxa de crescimento anual (AAGR)",
        "Média de citações por artigo",
        "Revistas únicas",
        "País mais produtivo",
        "Revista mais produtiva",
        "Keyword mais frequente"
      ),
      Valor = c(
        nrow(df),
        bases_incl,
        period_val,
        ifelse(is.na(cagr), "N/D", paste0(sprintf("%.2f", cagr), "%")),
        sprintf("%.1f", mean(df$TC_num, na.rm = TRUE)),
        dplyr::n_distinct(rv$df_merged$SO[!is.na(rv$df_merged$SO) & rv$df_merged$SO != ""]),
        ifelse(is.na(top_country), "N/D", top_country),
        ifelse(is.na(top_journal), "N/D", top_journal),
        ifelse(is.na(top_keyword), "N/D", top_keyword)
      ),
      Interpretacao = c(
        "Corpus final após deduplicação entre todas as bases configuradas",
        "Fontes atualmente carregadas no pipeline",
        "Janela temporal coberta pelo corpus final",
        "AAGR (média aritmética das taxas anuais) calculada sobre a produção anual",
        "Impacto médio do corpus com base nas citações disponíveis",
        "Diversidade de veículos de publicação",
        "País com maior produção segundo afiliações",
        "Periódico mais frequente no corpus",
        "Tema central mais recorrente nas keywords"
      ),
      stringsAsFactors = FALSE
    )
  })

  reading_queue <- reactive({
    df <- classified_df() %>%
      filter(prioridade %in% c("P1 - OBRIGATORIA", "P2 - RECOMENDADA", "P3 - OPCIONAL"))

    if (nrow(df) == 0) {
      return(data.frame(
        Semana = character(0), Prioridade = character(0), Autor = character(0), Ano = integer(0),
        Titulo = character(0), Revista = character(0), Citacoes = numeric(0), Score = numeric(0),
        Score_Tematico = numeric(0), Keywords = character(0), DOI = character(0),
        stringsAsFactors = FALSE
      ))
    }

    med_p2_bib <- median(df$score_bibliometrico[df$prioridade == "P2 - RECOMENDADA"], na.rm = TRUE)
    if (!is.finite(med_p2_bib)) med_p2_bib <- 0

    df %>%
      mutate(
        DT_UP = toupper(ifelse(is.na(DT), "", DT)),
        # Ordem importa: artigos fundadores (alto impacto) têm prioridade sobre a
        # classificação de tipo; reviews/sínteses que não são fundadores vão para a
        # semana 6, independentemente de serem P1, P2 ou P3.
        Semana = case_when(
          prioridade == "P1 - OBRIGATORIA" & TC_num >= 50 ~ "Semana 1 - Artigos fundadores",
          grepl("REVIEW|META-ANALYSIS", DT_UP)            ~ "Semana 6 - Reviews e sínteses",
          prioridade == "P1 - OBRIGATORIA"                ~ "Semana 2 - Obrigatórios recentes",
          prioridade == "P2 - RECOMENDADA" & score_bibliometrico >= med_p2_bib ~ "Semana 3 - Recomendados de maior impacto",
          prioridade == "P2 - RECOMENDADA"                ~ "Semana 4 - Recomendados complementares",
          prioridade == "P3 - OPCIONAL"                   ~ "Semana 5 - Opcionais estratégicos",
          TRUE ~ "Fora da fila"
        ),
        Semana = factor(
          Semana,
          levels = c(
            "Semana 1 - Artigos fundadores",
            "Semana 2 - Obrigatórios recentes",
            "Semana 3 - Recomendados de maior impacto",
            "Semana 4 - Recomendados complementares",
            "Semana 5 - Opcionais estratégicos",
            "Semana 6 - Reviews e sínteses"
          )
        )
      ) %>%
      arrange(Semana, desc(score_final), desc(TC_num), desc(PY_num)) %>%
      transmute(
        Semana = as.character(Semana),
        Prioridade = prioridade,
        Autor = clean_str(AU, 35),
        Ano = PY,
        Titulo = clean_str(TI, 120),
        Revista = clean_str(SO, 35),
        Citacoes = TC_num,
        Score = score_final,
        Score_Tematico = score_tematico,
        Keywords = clean_str(DE, 80),
        DOI = DI
      )
  })

  top_institutions <- reactive({
    req(rv$df_merged)
    top_institutions_df(rv$df_merged, top_n = 20)
  })


  keyword_network <- reactive({
    req(rv$net_kw)
    rv$net_kw
  })

  author_network <- reactive({
    req(rv$net_au)
    rv$net_au
  })

  country_network <- reactive({
    req(rv$net_co)
    rv$net_co
  })

  network_overview <- reactive({
    dplyr::bind_rows(
      network_summary_row(keyword_network(), "Keywords"),
      network_summary_row(author_network(), "Coautoria"),
      network_summary_row(country_network(), "Países")
    )
  })

  keyword_clusters_tbl <- reactive({
    cluster_terms_df(keyword_network(), top_terms = 7)
  })

  fila_filtrada <- reactive({
    fila <- reading_queue()
    if (is.null(input$filter_semana) || identical(input$filter_semana, "TODAS")) return(fila)
    fila %>% filter(Semana == input$filter_semana)
  })

  # ── Critérios ────────────────────────────────────────────────
  output$tabela_criterios <- renderDT({
    if (nrow(rv$criterios) == 0) {
      df_vazio <- criteria_empty_df()
      return(datatable(
        df_vazio,
        rownames = FALSE,
        colnames = c("ID","Nome","Peso","Palavras-chave"),
        options = list(dom = "t", language = list(emptyTable = "Nenhum critério adicionado."))
      ))
    }

    datatable(
      rv$criterios,
      editable = list(target = "cell", disable = list(columns = 0)),
      selection = "single",
      rownames = FALSE,
      colnames = c("ID","Nome","Peso","Palavras-chave"),
      options = list(pageLength = 15, dom = "t", scrollX = TRUE)
    )
  })

  observeEvent(input$tabela_criterios_cell_edit, {
    info <- input$tabela_criterios_cell_edit
    i <- info$row
    j <- info$col + 1

    if (j == 3) {
      rv$criterios[i, j] <- suppressWarnings(as.integer(info$value))
      if (is.na(rv$criterios[i, j])) rv$criterios[i, j] <- 1L
    } else {
      rv$criterios[i, j] <- info$value
    }
  })

  observeEvent(input$btn_add_crit, {
    req(input$novo_nome, input$novo_kw)
    rv$criterios <- rbind(
      rv$criterios,
      data.frame(
        id = paste0("T", nrow(rv$criterios) + 1),
        nome = input$novo_nome,
        peso = as.integer(input$novo_peso),
        keywords = input$novo_kw,
        stringsAsFactors = FALSE
      )
    )
    updateTextInput(session, "novo_nome", value = "")
    updateTextAreaInput(session, "novo_kw", value = "")
    showNotification(paste0("Critério '", input$novo_nome, "' adicionado!"), type = "message")
  })

  rebuild_ids <- function(df) {
    if (nrow(df) == 0) return(criteria_empty_df())
    df$id <- paste0("T", seq_len(nrow(df)))
    df
  }

  observeEvent(input$btn_substituir_crit_busca, {
    req(input$string_busca)
    novos <- build_criteria_from_search(input$string_busca)
    if (nrow(novos) == 0) {
      showNotification("Não foi possível extrair critérios da chave de busca.", type = "warning")
      return()
    }
    rv$criterios <- rebuild_ids(novos)
    showNotification(
      HTML("Critérios gerados. <b>Atenção:</b> os pesos foram atribuídos automaticamente por ordem de aparição na string — revise-os antes de executar o pipeline."),
      type = "warning", duration = 8
    )
  })

  observeEvent(input$btn_somar_crit_busca, {
    req(input$string_busca)
    novos <- build_criteria_from_search(input$string_busca)
    if (nrow(novos) == 0) {
      showNotification("Não foi possível extrair critérios da chave de busca.", type = "warning")
      return()
    }
    rv$criterios <- rebuild_ids(rbind(rv$criterios, novos))
    showNotification(
      HTML("Critérios adicionados. <b>Atenção:</b> os pesos foram atribuídos automaticamente por ordem de aparição na string — revise-os antes de executar o pipeline."),
      type = "warning", duration = 8
    )
  })

  observeEvent(input$btn_limpar_criterios, {
    rv$criterios <- criteria_empty_df()
    showNotification("Critérios temáticos limpos.", type = "message")
  })

  observeEvent(input$modo_eixo_tematico, {
    if (identical(input$modo_eixo_tematico, "manual") && nrow(rv$criterios) == 0) {
      showNotification("Modo manual ativo. Adicione seus critérios temáticos na tabela.", type = "message")
    }
  })

  # ── Carregar bases ───────────────────────────────────────────
  observeEvent(input$btn_carregar, {
    w <- Waiter$new(
      html = tagList(
        spin_flower(), tags$br(),
        tags$span("Carregando bases...", style = "color:white; font-size:16px;")
      ),
      color = "rgba(31,56,100,0.85)"
    )
    w$show()

    tryCatch({
      dfs <- list()
      rv$n_pubmed <- 0L
      rv$n_wos <- 0L
      rv$n_scopus <- 0L
      rv$n_extra <- 0L

      p <- get_path(input$file_pubmed)
      if (!is.null(p)) {
        log_add("Importando PubMed...")
        df <- convert2df(file = p, dbsource = "pubmed", format = "pubmed")
        rv$n_pubmed <- nrow(df)
        dfs[["pubmed"]] <- df
        log_add(paste0("  PubMed: ", nrow(df), " registros"))
      }

      p <- get_path(input$file_wos)
      if (!is.null(p)) {
        log_add("Importando Web of Science...")
        df <- convert2df(file = p, dbsource = "wos", format = "plaintext")
        rv$n_wos <- nrow(df)
        dfs[["wos"]] <- df
        log_add(paste0("  WoS: ", nrow(df), " registros"))
      }

      p <- get_path(input$file_scopus)
      if (!is.null(p)) {
        log_add("Importando Scopus...")
        df <- convert2df(file = p, dbsource = "scopus", format = input$scopus_format)
        rv$n_scopus <- nrow(df)
        dfs[["scopus"]] <- df
        log_add(paste0("  Scopus: ", nrow(df), " registros"))
      }

      p <- get_path(input$file_extra)
      if (isTRUE(input$usar_base_extra) && !is.null(p) && nchar(trimws(input$extra_dbname)) > 0) {
        log_add(paste0("Importando ", input$extra_dbname, "..."))
        df <- convert2df(file = p, dbsource = input$extra_dbsource, format = input$extra_format)
        rv$n_extra <- nrow(df)
        dfs[[input$extra_dbname]] <- df
        log_add(paste0("  ", input$extra_dbname, ": ", nrow(df), " registros"))
      }

      if (length(dfs) == 0) {
        w$hide()
        showNotification("Selecione ao menos um arquivo!", type = "error")
        return()
      }

      log_add("Mesclando e removendo duplicatas...")
      total <- sum(vapply(dfs, nrow, numeric(1)))
      merged <- if (length(dfs) == 1) {
        dfs[[1]]
      } else {
        do.call(mergeDbSources, c(dfs, list(remove.duplicated = TRUE)))
      }

      # Extrai o país dos autores (AU_CO) a partir das afiliações (C1).
      # convert2df/mergeDbSources não criam AU_CO automaticamente: sem este passo,
      # as análises de país e a rede de colaboração entre países ficam vazias ou
      # usam a afiliação completa (C1) como se fosse país.
      if (!"AU_CO" %in% names(merged)) {
        merged <- tryCatch(
          metaTagExtraction(merged, Field = "AU_CO", sep = ";"),
          error = function(e) {
            log_add(paste0("  Aviso: não foi possível extrair país dos autores (AU_CO): ", e$message))
            merged
          }
        )
      }

      rv$dup_rem <- total - nrow(merged)
      rv$df_merged <- merged
      rv$df_score <- NULL
      rv$results <- NULL
      rv$plots <- list()

      log_add(paste0("Total bruto: ", total))
      log_add(paste0("Duplicatas removidas: ", rv$dup_rem))
      log_add(paste0("Corpus final: ", nrow(merged), " artigos únicos"))
      log_add("Bases carregadas com sucesso!")

      w$hide()
      showNotification(paste0(nrow(merged), " artigos carregados!"), type = "message", duration = 4)
    }, error = function(e) {
      w$hide()
      log_add(paste0("ERRO: ", e$message))
      showNotification(paste0("Erro: ", e$message), type = "error", duration = 8)
    })
  })

  output$corpus_summary_boxes <- renderUI({
    req(rv$df_merged)
    fluidRow(
      infoBox("PubMed", rv$n_pubmed, icon = icon("book-medical"), color = "red", fill = TRUE),
      infoBox("WoS", rv$n_wos, icon = icon("flask"), color = "blue", fill = TRUE),
      infoBox("Scopus", rv$n_scopus, icon = icon("graduation-cap"), color = "yellow", fill = TRUE),
      infoBox("Corpus Final", nrow(rv$df_merged), icon = icon("database"), color = "green", fill = TRUE)
    )
  })

  output$status_sidebar <- renderUI({
    if (is.null(rv$df_merged)) {
      return(tags$span(style = "color:#E74C3C;", "Aguardando dados"))
    }
    if (is.null(rv$df_score)) {
      return(tags$span(style = "color:#C2410C;", paste0(nrow(rv$df_merged), " artigos carregados")))
    }
    df_cls <- tryCatch(classified_df(), error = function(e) NULL)
    p1_n <- if (is.null(df_cls)) 0 else sum(df_cls$prioridade == "P1 - OBRIGATORIA")
    tags$span(
      style = "color:#1E8449;",
      paste0(nrow(rv$df_merged), " artigos | ", p1_n, " P1")
    )
  })

  # ── Executar pipeline ────────────────────────────────────────
  observeEvent(input$btn_executar, {
    if (is.null(rv$df_merged)) {
      showNotification("Carregue e mescle ao menos uma base na aba '1. Tema e Bases' antes de executar o pipeline.", type = "error", duration = 6)
      updateTabItems(session, "sidebar", "bases")
      return()
    }

    if (!thresholds_are_valid(input$limiar_p1, input$limiar_p2, input$limiar_p3, input$limiar_removido)) {
      showNotification("Ajuste os limiares para que P1 > P2 > P3 > Removidos ≥ 0 antes de executar.", type = "error", duration = 6)
      updateTabItems(session, "sidebar", "pipeline")
      return()
    }

    usar_eixo <- identical(input$usar_eixo_tematico, "sim")
    if (usar_eixo && nrow(rv$criterios) == 0) {
      showNotification("O eixo temático está ativo, mas não há critérios temáticos definidos.", type = "error")
      updateTabItems(session, "sidebar", "criterios")
      return()
    }

    w <- Waiter$new(
      html = tagList(
        spin_flower(), tags$br(),
        tags$span("Executando pipeline...", style = "color:white; font-size:16px;")
      ),
      color = "rgba(31,56,100,0.9)"
    )
    w$show()

    tryCatch({
      df <- rv$df_merged

      log_add("[1/4] Preparando corpus...")
      df <- df %>%
        mutate(
          TI = ifelse(is.na(TI), "", as.character(TI)),
          AB = ifelse(is.na(AB), "", as.character(AB)),
          DE = ifelse(is.na(DE), "", as.character(DE)),
          SO = ifelse(is.na(SO), "", as.character(SO)),
          DT = ifelse(is.na(DT), "", as.character(DT)),
          AU = ifelse(is.na(AU), "", as.character(AU)),
          DI = ifelse(is.na(DI), "", as.character(DI))
        )

      # ── Bibliometria ─────────────────────────────────────────
      log_add("[2/4] Análise bibliométrica...")
      rv$results <- biblioAnalysis(df, sep = ";")

      annual_df <- df %>%
        mutate(Year = suppressWarnings(as.numeric(PY))) %>%
        filter(!is.na(Year), Year >= input$ano_inicial) %>%
        count(Year, name = "N") %>%
        arrange(Year)

      top_j <- df %>%
        filter(!is.na(SO), SO != "") %>%
        count(SO, name = "N") %>%
        arrange(desc(N)) %>%
        head(20) %>%
        mutate(SO = substr(SO, 1, 35)) %>%
        arrange(N)

      top_co <- tryCatch({
        co <- get_country_col(df)
        if (length(co) == 0 || is.na(co)) stop("sem coluna de país identificável")
        df %>%
          filter(!is.na(.data[[co]]), as.character(.data[[co]]) != "") %>%
          mutate(CO = strsplit(as.character(.data[[co]]), ";")) %>%
          unnest(CO) %>%
          mutate(CO = trimws(toupper(CO))) %>%
          filter(CO != "", nchar(CO) <= 30) %>%
          count(CO, name = "N") %>%
          arrange(desc(N)) %>%
          head(20) %>%
          mutate(hl = CO %in% c("BRAZIL", "BRASIL")) %>%
          arrange(N)
      }, error = function(e) {
        log_add(paste0("  Aviso países: ", e$message))
        data.frame(CO = "N/A", N = 1L, hl = FALSE)
      })

      top_au <- df %>%
        filter(!is.na(AU), AU != "") %>%
        mutate(A = strsplit(AU, ";")) %>%
        unnest(A) %>%
        mutate(A = trimws(A)) %>%
        filter(A != "") %>%
        count(A, name = "N") %>%
        arrange(desc(N)) %>%
        head(20) %>%
        arrange(N)

      top_kw <- df %>%
        filter(!is.na(DE), DE != "") %>%
        mutate(K = strsplit(toupper(DE), ";")) %>%
        unnest(K) %>%
        mutate(K = trimws(K)) %>%
        filter(K != "") %>%
        count(K, name = "N") %>%
        arrange(desc(N)) %>%
        head(25) %>%
        arrange(N)

      bar_pl <- function(dat, xc, yc, fc, tt) {
        ggplot(dat, aes(x = reorder(.data[[xc]], .data[[yc]]), y = .data[[yc]])) +
          geom_col(fill = fc) +
          geom_text(aes(label = .data[[yc]]), hjust = -0.2, size = 2.8) +
          coord_flip() +
          scale_y_continuous(expand = expansion(mult = c(0, .15))) +
          labs(title = tt, x = "", y = "") +
          theme_minimal(base_size = 10) +
          theme(
            plot.title = element_text(face = "bold", color = "#1F3864", size = 11),
            panel.grid.minor = element_blank()
          )
      }

      rv$plots$annual <- ggplot(annual_df, aes(x = Year, y = N)) +
        geom_col(fill = "#1E40AF", width = 0.75) +
        geom_smooth(method = "loess", se = FALSE, color = "#B91C1C", linewidth = 1.2) +
        geom_text(aes(label = N), vjust = -0.4, size = 2.8, color = "#1F3864", fontface = "bold") +
        scale_x_continuous(breaks = seq(input$ano_inicial, 2030, 2)) +
        labs(
          title = "Annual Scientific Production",
          subtitle = paste0("n=", nrow(df), " articles — ", safe_theme_title(input$tema_titulo)),
          x = "Year", y = "Publications"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          plot.title = element_text(face = "bold", color = "#1F3864"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank()
        )

      rv$plots$journals <- bar_pl(top_j, "SO", "N", "#E74C3C", "Top 20 Journals")
      rv$plots$countries <- ggplot(top_co, aes(x = reorder(CO, N), y = N, fill = hl)) +
        geom_col() +
        scale_fill_manual(values = c("FALSE" = "#3498DB", "TRUE" = "#27AE60"), guide = "none") +
        geom_text(aes(label = N), hjust = -0.2, size = 2.8) +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, .15))) +
        labs(title = "Top Countries", subtitle = "Green = Brazil", x = "", y = "") +
        theme_minimal(base_size = 10) +
        theme(plot.title = element_text(face = "bold", color = "#1F3864"))
      rv$plots$authors <- bar_pl(top_au, "A", "N", "#1ABC9C", "Top 20 Authors")
      rv$plots$keywords <- bar_pl(top_kw, "K", "N", "#9B59B6", "Top 25 Keywords")

      log_add("  Figuras geradas.")

      # ── Ranking ──────────────────────────────────────────────
      log_add("[3/4] Calculando scores...")

      crit <- rv$criterios
      usar_eixo <- identical(input$usar_eixo_tematico, "sim")
      peso_t <- if (usar_eixo) input$peso_tematico / 100 else 0
      peso_b <- 1 - peso_t
      max_t <- if (usar_eixo && nrow(crit) > 0) sum(crit$peso) else 1
      ano_ref <- format(input$data_busca, "%Y")
      ano_ref <- suppressWarnings(as.numeric(ano_ref))
      if (is.na(ano_ref)) ano_ref <- as.numeric(format(Sys.Date(), "%Y"))

      usar_revistas <- isTRUE(input$usar_score_revistas)
      rev_top   <- if (usar_revistas && nchar(trimws(input$revistas_top))   > 0) input$revistas_top   else "XXXXXXXX_NAO_EXISTE"
      rev_med   <- if (usar_revistas && nchar(trimws(input$revistas_med))   > 0) input$revistas_med   else "XXXXXXXX_NAO_EXISTE"
      rev_tier3 <- if (usar_revistas && nchar(trimws(input$revistas_tier3)) > 0) input$revistas_tier3 else "XXXXXXXX_NAO_EXISTE"
      rev_tier4 <- if (usar_revistas && nchar(trimws(input$revistas_tier4)) > 0) input$revistas_tier4 else "XXXXXXXX_NAO_EXISTE"

      df2 <- df %>%
        mutate(
          TEXTO_ANALISE = toupper(paste(TI, DE, AB, sep = " ")),
          TC_num = suppressWarnings(as.numeric(TC)),
          TC_num = ifelse(is.na(TC_num), 0, TC_num),
          PY_num = suppressWarnings(as.numeric(PY))
        )

      # Imputar PY_num ausente com a mediana do corpus (não com ano_inicial)
      py_vals   <- df2$PY_num[!is.na(df2$PY_num)]
      py_median <- if (length(py_vals) > 0) median(py_vals) else as.numeric(format(input$data_busca, "%Y"))
      rv$py_median <- py_median
      n_sem_ano <- sum(is.na(df2$PY_num))
      rv$n_sem_ano <- n_sem_ano
      if (n_sem_ano > 0) log_add(paste0("  Aviso: ", n_sem_ano, " artigo(s) sem ano de publicação — PY imputado com mediana do corpus (", py_median, ")."))
      df2 <- df2 %>%
        mutate(
          PY_num = ifelse(is.na(PY_num), py_median, PY_num),
          idade_artigo = pmax(1, ano_ref - PY_num + 1),
          citacoes_por_ano = round(TC_num / idade_artigo, 2)
        )

      if (usar_eixo && nrow(crit) > 0) {
        # Helper: retorna vetor lógico de matches para um conjunto de keywords.
        # Termos de uma palavra usam word-boundary (\b); multi-palavra usam fixed().
        match_kws <- function(text_vec, kws) {
          result <- rep(FALSE, length(text_vec))
          single_kws <- kws[!grepl("\\s", kws, perl = TRUE)]
          multi_kws  <- kws[ grepl("\\s", kws, perl = TRUE)]
          if (length(single_kws) > 0) {
            # Escapa metacaracteres de regex: termos como "CA2+", "C++" ou "P53(+)"
            # quebrariam o grepl se inseridos crus no padrão.
            esc <- escape_regex(single_kws)
            pat <- paste0("(?<![A-Z0-9])(", paste(esc, collapse = "|"), ")(?![A-Z0-9])")
            result <- result | tryCatch(
              grepl(pat, text_vec, perl = TRUE),
              error = function(e) FALSE
            )
          }
          for (mw in multi_kws) {
            result <- result | grepl(mw, text_vec, fixed = TRUE)
          }
          result
        }

        for (i in seq_len(nrow(crit))) {
          kws <- trimws(unlist(strsplit(crit$keywords[i], ";")))
          kws <- kws[nchar(kws) > 0]
          kws <- toupper(kws)

          if (length(kws) == 0) {
            df2[[paste0("t_", crit$id[i])]] <- 0L
          } else {
            # Score graduado por campo: TI = 100%, DE = 80%, AB = 50% do peso
            hit_ti <- match_kws(toupper(df2$TI), kws)
            hit_de <- match_kws(toupper(df2$DE), kws)
            hit_ab <- match_kws(toupper(df2$AB), kws)

            df2[[paste0("t_", crit$id[i])]] <- dplyr::case_when(
              hit_ti                         ~ crit$peso[i],
              hit_de & !hit_ti               ~ round(crit$peso[i] * 0.8),
              hit_ab & !hit_ti & !hit_de     ~ round(crit$peso[i] * 0.5),
              TRUE                           ~ 0L
            )
          }
        }
        t_cols <- paste0("t_", crit$id)
        df2 <- df2 %>% mutate(score_tematico = rowSums(select(., all_of(t_cols)), na.rm = TRUE))
      } else {
        df2 <- df2 %>% mutate(score_tematico = 0)
      }

      df2 <- df2 %>%
        mutate(
          score_citacoes = if (identical(input$modo_citacoes_tempo, "ponderado")) {
            score_citations_time_adjusted(citacoes_por_ano)
          } else {
            score_citations_raw(TC_num)
          },
          score_recencia = if (isTRUE(input$usar_recencia)) score_recency_fun(PY_num, ref_year = as.integer(format(input$data_busca, "%Y"))) else 0,
          score_revista = if (usar_revistas) {
            case_when(
              safe_grepl(rev_top,   toupper(SO)) ~ 5,
              safe_grepl(rev_med,   toupper(SO)) ~ 4,
              safe_grepl(rev_tier3, toupper(SO)) ~ 3,
              safe_grepl(rev_tier4, toupper(SO)) ~ 2,
              TRUE ~ 1
            )
          } else {
            0
          },
          score_tipo = case_when(
            grepl("SYSTEMATIC REVIEW|REVIEW", toupper(DT)) ~ 3,
            grepl("ARTICLE|JOURNAL ARTICLE", toupper(DT)) ~ 2,
            TRUE ~ 1
          )
        )

      max_bib <- 10 + (if (isTRUE(input$usar_recencia)) 5 else 0) + (if (usar_revistas) 5 else 0) + 3

      df2 <- df2 %>%
        mutate(
          score_bibliometrico = score_citacoes + score_recencia + score_revista + score_tipo,
          componente_tematico = if (usar_eixo) (score_tematico / max_t * 100) * peso_t else 0,
          componente_bibliometrico = (score_bibliometrico / max_bib * 100) * peso_b,
          score_final = round(componente_tematico + componente_bibliometrico, 1)
        )

      rv$df_score <- df2

      log_add("[4/4] Calculando redes bibliométricas...")
      seed_val <- as.integer(input$louvain_seed)
      rv$net_kw <- build_network_bundle(df, analysis = "co-occurrences", network = "keywords", top_n = 40, seed = seed_val)
      rv$net_au <- build_network_bundle(df, analysis = "collaboration",   network = "authors",   top_n = 35, seed = seed_val)
      rv$net_co <- build_network_bundle(df, analysis = "collaboration",   network = "countries", top_n = 30, seed = seed_val)
      log_add("  Redes geradas e cacheadas.")

      # Reprodutibilidade: versão dos pacotes-chave no log
      pkg_vers <- tryCatch({
        paste0(
          "bibliometrix=", packageVersion("bibliometrix"), " | ",
          "igraph=",       packageVersion("igraph"),       " | ",
          "R=",            paste(R.version$major, R.version$minor, sep = ".")
        )
      }, error = function(e) "versão não disponível")
      log_add(paste0("  Pacotes: ", pkg_vers, " | seed=", seed_val, " (Louvain)"))

      dist <- apply_priority_labels(df2, input$limiar_p1, input$limiar_p2, input$limiar_p3, input$limiar_removido) %>%
        count(prioridade) %>%
        mutate(pct = round(n / sum(n) * 100, 1)) %>%
        arrange(prioridade)

      for (i in seq_len(nrow(dist))) {
        log_add(paste0("  ", dist$prioridade[i], ": ", dist$n[i], " (", dist$pct[i], "%)"))
      }

      log_add(paste0("  Modo eixo temático: ", if (usar_eixo) "ATIVO (score graduado por campo: TI>DE>AB)" else "DESATIVADO"))
      log_add(paste0(
        "  Citações: ",
        if (identical(input$modo_citacoes_tempo, "ponderado")) "ponderadas por citações/ano" else "brutas"
      ))
      log_add(paste0("  Score de recência: ", if (isTRUE(input$usar_recencia)) "ATIVO" else "DESATIVADO"))
      log_add(paste0("  Score de revistas: ", if (usar_revistas) "ATIVO" else "DESATIVADO"))

      log_add("[5/5] Pipeline concluído!")
      w$hide()
      showNotification("Pipeline executado com sucesso!", type = "message")
      updateTabItems(session, "sidebar", "dashboard")

    }, error = function(e) {
      w$hide()
      log_add(paste0("ERRO: ", e$message))
      showNotification(paste0("Erro: ", e$message), type = "error", duration = 8)
    })
  })

  output$log_output <- renderText(paste(rv$log, collapse = "\n"))

  # ── Plots ────────────────────────────────────────────────────
  output$plot_annual      <- renderPlot({ req(rv$plots$annual); rv$plots$annual })
  output$dash_plot_annual <- renderPlot({ req(rv$plots$annual); rv$plots$annual })
  output$plot_journals  <- renderPlot({ req(rv$plots$journals); rv$plots$journals })
  output$plot_countries <- renderPlot({ req(rv$plots$countries); rv$plots$countries })
  output$plot_authors   <- renderPlot({ req(rv$plots$authors); rv$plots$authors })
  output$plot_keywords  <- renderPlot({ req(rv$plots$keywords); rv$plots$keywords })

  output$plot_institutions <- renderPlot({
    inst <- top_institutions()
    validate(need(nrow(inst) > 0, "Sem dados institucionais suficientes para plotar."))
    ggplot(inst, aes(x = reorder(INST, N), y = N)) +
      geom_col(fill = "#5B6CFA") +
      geom_text(aes(label = N), hjust = -0.2, size = 2.8) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, .15))) +
      labs(title = "Top 20 Institutions", x = "", y = "") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(face = "bold", color = "#1F3864"))
  })

  output$plot_citations_dist <- renderPlot({
    df <- classified_df()
    ggplot(df, aes(x = TC_num)) +
      geom_histogram(bins = 30, fill = "#7C3AED", color = "white") +
      labs(title = "Citation Distribution", x = "Citações", y = "Número de artigos") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(face = "bold", color = "#1F3864"))
  })

  score_dist_plot <- function() {
    df <- classified_df()
    ggplot(df, aes(x = score_final, fill = prioridade)) +
      geom_histogram(bins = 20, alpha = .85, color = "white") +
      scale_fill_manual(values = c("P1 - OBRIGATORIA"="#1E40AF","P2 - RECOMENDADA"="#1E8449","P3 - OPCIONAL"="#C2410C","ABAIXO DO CORTE"="#E67E22","P4 - REMOVIDO"="#B91C1C")) +
      labs(title = "Final Score Distribution", x = "Score final", y = "Número de artigos") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(face = "bold", color = "#1F3864"),
            legend.title = element_blank())
  }
  output$plot_score_dist      <- renderPlot(score_dist_plot())
  output$dash_plot_score_dist <- renderPlot(score_dist_plot())

  output$plot_score_scatter <- renderPlot({
    df <- classified_df()
    ggplot(df, aes(x = score_tematico, y = score_bibliometrico, color = prioridade, size = pmax(TC_num, 1))) +
      geom_point(alpha = .75) +
      scale_color_manual(values = c("P1 - OBRIGATORIA"="#1E40AF","P2 - RECOMENDADA"="#1E8449","P3 - OPCIONAL"="#C2410C","ABAIXO DO CORTE"="#E67E22","P4 - REMOVIDO"="#B91C1C")) +
      scale_size_continuous(range = c(1.5, 8)) +
      labs(title = "Thematic Score × Bibliometric Score", x = "Score temático", y = "Score bibliométrico") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(face = "bold", color = "#1F3864"),
            legend.title = element_blank())
  })


  output$plot_kw_network <- renderPlot({
    plot_network_bundle(keyword_network(), "Rede de coocorrência de keywords")
  })

  output$plot_author_network <- renderPlot({
    plot_network_bundle(author_network(), "Rede de coautoria")
  })

  output$plot_country_network <- renderPlot({
    plot_network_bundle(country_network(), "Rede de colaboração entre países")
  })

  output$table_network_metrics <- renderDT({
    datatable(
      network_overview(),
      rownames = FALSE,
      options = list(pageLength = 10, dom = "t", scrollX = TRUE)
    )
  })

  output$table_kw_clusters <- renderDT({
    datatable(
      keyword_clusters_tbl(),
      rownames = FALSE,
      options = list(pageLength = 10, dom = "t", scrollX = TRUE)
    )
  })

  output$dashboard_header <- renderUI({
    req(classified_df())
    peso_txt <- if (identical(input$usar_eixo_tematico, "sim")) {
      paste0(input$peso_tematico, "% temático + ", 100 - input$peso_tematico, "% bibliométrico")
    } else {
      "100% bibliométrico"
    }

    bases_txt <- paste(c(
      paste0("PubMed (n=", rv$n_pubmed, ")"),
      paste0("Scopus (n=", rv$n_scopus, ")"),
      paste0("Web of Science (n=", rv$n_wos, ")"),
      if (rv$n_extra > 0) paste0(input$extra_dbname, " (n=", rv$n_extra, ")") else NULL
    ), collapse = " + ")

    div(
      class = "clean-card soft",
      tags$h3(style = "margin-top:0; color:#0B1F3F; font-weight:800;",
              paste0(safe_theme_title(input$tema_titulo), " — Article Ranking Dashboard")),
      tags$p(style = "margin-bottom:0; color:#475569;",
             paste0(bases_txt, "  |  Corpus: ", nrow(classified_df()), " artigos  |  Score: ", peso_txt))
    )
  })

  output$dashboard_priority_boxes <- renderUI({
    df <- classified_df()
    fluidRow(
      infoBox("P1 - Obrigatória", sum(df$prioridade == "P1 - OBRIGATORIA"), subtitle = paste0("Score ≥ ", input$limiar_p1), icon = icon("star"), color = "blue", fill = TRUE),
      infoBox("P2 - Recomendada", sum(df$prioridade == "P2 - RECOMENDADA"), subtitle = paste0("Score ", input$limiar_p2, "–", input$limiar_p1), icon = icon("thumbs-up"), color = "green", fill = TRUE),
      infoBox("P3 - Opcional", sum(df$prioridade == "P3 - OPCIONAL"), subtitle = paste0("Score ", input$limiar_p3, "–", input$limiar_p2), icon = icon("bookmark"), color = "yellow", fill = TRUE),
      infoBox("P4 - Removidos", sum(df$prioridade == "P4 - REMOVIDO"), subtitle = paste0("Score < ", input$limiar_removido), icon = icon("trash"), color = "red", fill = TRUE)
    )
  })

  output$dashboard_table <- renderDT({
    datatable(
      dashboard_indicators(),
      rownames = FALSE,
      options = list(pageLength = 9, dom = "t", scrollX = TRUE)
    )
  })

  output$plot_class_dist <- renderPlot({
    df <- classified_df() %>%
      count(prioridade, name = "N") %>%
      mutate(prioridade = factor(prioridade, levels = c("P1 - OBRIGATORIA", "P2 - RECOMENDADA", "P3 - OPCIONAL", "ABAIXO DO CORTE", "P4 - REMOVIDO")))
    ggplot(df, aes(x = prioridade, y = N, fill = prioridade)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("P1 - OBRIGATORIA"="#1E40AF","P2 - RECOMENDADA"="#1E8449","P3 - OPCIONAL"="#C2410C","ABAIXO DO CORTE"="#E67E22","P4 - REMOVIDO"="#B91C1C")) +
      geom_text(aes(label = N), vjust = -0.3, size = 3) +
      scale_y_continuous(expand = expansion(mult = c(0, .12))) +
      labs(x = "", y = "Número de artigos") +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })

  output$table_dashboard_top <- renderDT({
    df <- classified_df() %>%
      filter(prioridade == "P1 - OBRIGATORIA") %>%
      arrange(desc(score_final), desc(TC_num)) %>%
      transmute(
        Autor = clean_str(AU, 35),
        Ano = PY,
        Titulo = clean_str(TI, 110),
        Revista = clean_str(SO, 30),
        Citacoes = TC_num,
        Score = score_final,
        DOI = DI
      ) %>%
      head(15)

    datatable(df, rownames = FALSE, options = list(pageLength = 15, dom = "t", scrollX = TRUE))
  })

  output$fila_title <- renderUI({
    paste0("Fila semanal (", nrow(fila_filtrada()), " artigos)")
  })

  output$table_fila <- renderDT({
    fila <- fila_filtrada()
    datatable(
      fila,
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, dom = "lfrtip")
    ) %>%
      formatStyle(
        "Prioridade",
        backgroundColor = styleEqual(
          c("P1 - OBRIGATORIA", "P2 - RECOMENDADA", "P3 - OPCIONAL"),
          c("#DCEAF8", "#DFF2E5", "#FDEBD0")
        ),
        color = styleEqual(
          c("P1 - OBRIGATORIA", "P2 - RECOMENDADA", "P3 - OPCIONAL"),
          c("#1E40AF", "#1E8449", "#C2410C")
        ),
        fontWeight = "bold"
      )
  })

  # ── Interpretações ───────────────────────────────────────────
  output$interp_annual <- renderText({
    req(rv$df_merged)
    anos <- suppressWarnings(as.numeric(rv$df_merged$PY))
    ni <- sum(anos == input$ano_inicial, na.rm = TRUE)
    nmax_ano <- if (length(na.omit(anos)) > 0) max(anos, na.rm = TRUE) else input$ano_inicial
    nmax <- sum(anos == nmax_ano, na.rm = TRUE)
    paste0(
      "A produção foi de ", ni, " artigos em ", input$ano_inicial,
      " e de ", nmax, " artigos em ", nmax_ano, "."
    )
  })

  output$interp_journals <- renderText({
    req(rv$df_merged)
    top <- rv$df_merged %>%
      filter(!is.na(SO), SO != "") %>%
      count(SO) %>%
      arrange(desc(n)) %>%
      head(1) %>%
      pull(SO)
    if (length(top) == 0) return("Não foi possível identificar um periódico dominante.")
    paste0("A revista mais produtiva no corpus é ", top, ".")
  })

  output$interp_countries <- renderText({
    req(rv$df_merged)
    co <- get_country_col(rv$df_merged)
    if (length(co) == 0 || is.na(co)) return("Dados de país não disponíveis neste corpus.")
    nb <- rv$df_merged %>%
      filter(grepl("BRAZIL|BRASIL", toupper(as.character(.data[[co]])))) %>%
      nrow()
    pct <- round(nb / nrow(rv$df_merged) * 100, 1)
    paste0("Brasil contribui com ", pct, "% (", nb, " de ", nrow(rv$df_merged), " artigos).")
  })

  output$interp_keywords <- renderText({
    "As keywords mostram os clusters temáticos dominantes e ajudam a refinar o eixo temático do ranking."
  })

  # ── Info boxes ───────────────────────────────────────────────
  output$result_info_boxes <- renderUI({
    df <- classified_df()
    fluidRow(
      infoBox("P1 Obrigatória", sum(df$prioridade == "P1 - OBRIGATORIA"), icon = icon("star"), color = "blue", fill = TRUE),
      infoBox("P2 Recomendada", sum(df$prioridade == "P2 - RECOMENDADA"), icon = icon("thumbs-up"), color = "green", fill = TRUE),
      infoBox("P3 Opcional", sum(df$prioridade == "P3 - OPCIONAL"), icon = icon("bookmark"), color = "yellow", fill = TRUE),
      infoBox("Abaixo do corte", sum(df$prioridade == "ABAIXO DO CORTE"), icon = icon("minus-circle"), color = "orange", fill = TRUE),
      infoBox("P4 - Removidos", sum(df$prioridade == "P4 - REMOVIDO"), icon = icon("trash"), color = "red", fill = TRUE)
    )
  })

  # ── Tabela artigos ───────────────────────────────────────────
  art_filt <- reactive({
    df <- classified_df()

    if (!identical(input$filter_prio, "TODOS")) {
      df <- df %>% filter(prioridade == input$filter_prio)
    }

    df <- df %>%
      filter(score_final >= input$filter_score, TC_num >= input$filter_cit)

    if (nchar(trimws(input$filter_txt)) > 0) {
      pat <- toupper(trimws(input$filter_txt))
      df <- df %>% filter(grepl(pat, toupper(TI)) | grepl(pat, toupper(DE)) | grepl(pat, toupper(AB)))
    }

    df %>%
      arrange(desc(score_final)) %>%
      transmute(
        Classe = prioridade,
        Autor = clean_str(AU, 30),
        Ano = PY,
        Titulo = clean_str(TI, 100),
        Revista = clean_str(SO, 30),
        Citacoes = TC_num,
        Citacoes_por_ano = citacoes_por_ano,
        Score = score_final,
        DOI = DI
      )
  })

  output$art_title <- renderUI({
    n <- if (!is.null(art_filt())) nrow(art_filt()) else 0
    paste0("Artigos filtrados (", n, ")")
  })

  output$table_artigos <- renderDT({
    req(art_filt())
    datatable(
      art_filt(),
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, dom = "lfrtip")
    ) %>%
      formatStyle(
        "Classe",
        backgroundColor = styleEqual(
          c("P1 - OBRIGATORIA","P2 - RECOMENDADA","P3 - OPCIONAL","ABAIXO DO CORTE","P4 - REMOVIDO"),
          c("#DCEAF8","#DFF2E5","#FDEBD0","#FEF3E8","#FADBD8")
        ),
        color = styleEqual(
          c("P1 - OBRIGATORIA","P2 - RECOMENDADA","P3 - OPCIONAL","ABAIXO DO CORTE","P4 - REMOVIDO"),
          c("#1E40AF","#1E8449","#C2410C","#E67E22","#B91C1C")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Score",
        background = styleColorBar(c(0, 100), "#BDD7EE"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$table_cited <- renderDT({
    req(rv$df_merged)
    rv$df_merged %>%
      mutate(
        TC_num = suppressWarnings(as.numeric(TC)),
        TC_num = ifelse(is.na(TC_num), 0, TC_num)
      ) %>%
      arrange(desc(TC_num)) %>%
      select(AU, PY, TI, SO, TC_num) %>%
      head(20) %>%
      mutate(AU = substr(AU, 1, 30), TI = substr(TI, 1, 80), SO = substr(SO, 1, 30)) %>%
      datatable(
        rownames = FALSE,
        colnames = c("Autores", "Ano", "Título", "Revista", "Citações"),
        options = list(pageLength = 20, scrollX = TRUE, dom = "t")
      )
  })

  # ── Texto de metodologia ─────────────────────────────────────
  observeEvent(input$btn_gerar_metod, {
    req(rv$df_merged)
    if (is.null(rv$df_score)) {
      showNotification("Execute o pipeline antes de gerar o texto metodológico.", type = "error", duration = 6)
      return()
    }

    df <- rv$df_merged
    crit <- rv$criterios
    tema <- if (isTRUE(input$habilitar_tema) && nchar(trimws(input$tema_titulo)) > 0) input$tema_titulo else "[tema não definido]"
    autor <- if (isTRUE(input$habilitar_tema)) input$autor_nome else ""
    inst <- if (isTRUE(input$habilitar_tema)) input$instituicao else ""
    data_b <- format(input$data_busca, "%B %Y")
    kw_str <- input$string_busca
    usar_eixo <- identical(input$usar_eixo_tematico, "sim")
    pt <- if (usar_eixo) input$peso_tematico else 0
    pb <- 100 - pt
    lp1 <- input$limiar_p1
    lp2 <- input$limiar_p2
    lp3 <- input$limiar_p3
    n_tot <- nrow(df)
    n_dup <- rv$dup_rem
    df_cls <- tryCatch(classified_df(), error = function(e) NULL)
    p1n <- if (!is.null(df_cls)) sum(df_cls$prioridade == "P1 - OBRIGATORIA") else "—"
    p2n <- if (!is.null(df_cls)) sum(df_cls$prioridade == "P2 - RECOMENDADA") else "—"
    p3n <- if (!is.null(df_cls)) sum(df_cls$prioridade == "P3 - OPCIONAL") else "—"
    p4n <- if (!is.null(df_cls)) sum(df_cls$prioridade == "P4 - REMOVIDO") else "—"
    cutn <- if (!is.null(df_cls)) sum(df_cls$prioridade == "ABAIXO DO CORTE") else "—"

    bases_str <- paste(c(
      if (rv$n_pubmed > 0) paste0("PubMed (n=", rv$n_pubmed, ")") else NULL,
      if (rv$n_wos > 0) paste0("Web of Science (n=", rv$n_wos, ")") else NULL,
      if (rv$n_scopus > 0) paste0("Scopus (n=", rv$n_scopus, ")") else NULL,
      if (rv$n_extra > 0) paste0("Outra base (n=", rv$n_extra, ")") else NULL
    ), collapse = ", ")

    crit_txt <- if (usar_eixo && nrow(crit) > 0) {
      paste(
        sapply(seq_len(nrow(crit)), function(i) {
          paste0("(", i, ") ", crit$nome[i], " [peso: ", crit$peso[i], "; keywords: ", substr(crit$keywords[i], 1, 80), "]")
        }),
        collapse = "; "
      )
    } else {
      "eixo temático desativado"
    }

    cit_mode_pt <- if (identical(input$modo_citacoes_tempo, "ponderado")) {
      "citações ponderadas aproximadamente por citações/ano"
    } else {
      "citações brutas sem ponderação temporal"
    }

    cit_mode_en <- if (identical(input$modo_citacoes_tempo, "ponderado")) {
      "citation scores approximately normalized by citations per year"
    } else {
      "raw citation counts without temporal normalization"
    }

    rec_pt <- if (isTRUE(input$usar_recencia)) {
      "A recência também foi incorporada ao componente bibliométrico."
    } else {
      "A recência não foi incorporada como componente explícito do score."
    }

    rec_en <- if (isTRUE(input$usar_recencia)) {
      "Recency was also incorporated into the bibliometric component."
    } else {
      "Recency was not included as an explicit score component."
    }

    if (input$metod_idioma == "en") {
      txt <- paste0(
        "2. METHODS\n\n",
        "2.1 Search strategy\n\n",
        "A bibliographic search on the topic '", tema, "' was conducted in ", data_b,
        " across the following electronic databases: ", bases_str, ". ",
        "The search string used to retrieve records was: ", kw_str, ". ",
        "The search interval covered publications from ", input$ano_inicial, " to ",
        format(input$data_busca, "%Y"), ".\n\n",

        "2.2 Deduplication and corpus assembly\n\n",
        "Retrieved records were imported into R (v", paste(R.version$major, R.version$minor, sep = "."),
        ") using the bibliometrix package and merged with automatic duplicate removal. ",
        "A total of ", rv$n_pubmed + rv$n_wos + rv$n_scopus + rv$n_extra, " records were retrieved, of which ",
        n_dup, " were removed as duplicates. The final corpus comprised ", n_tot, " unique records.",
        if (!is.na(rv$py_median) && rv$n_sem_ano > 0) paste0(" A total of ", rv$n_sem_ano, " records lacked publication year and were assigned the corpus median year (", rv$py_median, ") for scoring purposes only.") else "",
        "\n\n",

        "2.3 Bibliometric analysis\n\n",
        "Descriptive bibliometric analyses included annual production (trend assessed by the arithmetic average annual growth rate, AAGR), ",
        "most productive journals, countries, authors, and keyword frequencies. ",
        "Co-occurrence and collaboration networks were computed via the bibliometrix package, clustered using the Louvain algorithm (igraph package, seed = ", input$louvain_seed, " for reproducibility). ",
        "Visualizations were produced using ggplot2.\n\n",

        "2.4 Article prioritization and scoring\n\n",
        "Each article received a composite relevance score. ",
        if (usar_eixo) {
          paste0(
            "The final score combined a thematic component (", pt, "% weight) and a bibliometric component (", pb, "% weight). ",
            "The thematic component was based on ", nrow(crit), " predefined thematic criteria: ", crit_txt, ". ",
            "Thematic scoring was field-weighted: a keyword match in the title received full criterion weight; ",
            "a match only in the author keywords received 80%; a match only in the abstract received 50%.",
            " Multi-word terms were matched using exact string search; single-word terms used word-boundary regular expressions. "
          )
        } else {
          "The final score was based exclusively on bibliometric criteria, with no thematic component. "
        },
        "The bibliometric component incorporated ", cit_mode_en, ", journal tier, and document type. ",
        rec_en, " ",
        "Articles were classified as mandatory reading (P1, score ≥ ", lp1, "; n=", p1n,
        "), recommended reading (P2, score ", lp2, "–", lp1, "; n=", p2n,
        "), optional reading (P3, score ", lp3, "–", lp2, "; n=", p3n,
        "), below cutoff (scores between ", input$limiar_removido, " and ", lp3, "; n=", cutn,
        "), or removed (P4, score < ", input$limiar_removido, "; n=", p4n, "). ",
        "All analyses were conducted in R using LIRA (Literature Integration, Ranking & Analysis, v1.0.0), an open Shiny pipeline for bibliometric screening and prioritization",
        if (nchar(autor) > 0) paste0(", operated by ", autor, if (nchar(inst) > 0) paste0(" (", inst, ")") else "", ".") else ".",
        " Analysis parameters are reported in the Supplementary 'Configurações da Análise' sheet to ensure reproducibility."
      )
    } else {
      txt <- paste0(
        "2. METODOLOGIA\n\n",
        "2.1 Estratégia de busca\n\n",
        "Uma busca bibliográfica sobre o tema '", tema, "' foi conduzida em ", data_b,
        " nas seguintes bases de dados eletrônicas: ", bases_str, ". ",
        "A string de busca utilizada para recuperação dos registros foi: ", kw_str, ". ",
        "O período de busca compreendeu publicações entre ", input$ano_inicial, " e ",
        format(input$data_busca, "%Y"), ".\n\n",

        "2.2 Deduplicação e montagem do corpus\n\n",
        "Os registros recuperados foram importados para o R (v", paste(R.version$major, R.version$minor, sep = "."),
        ") utilizando o pacote bibliometrix e mesclados com remoção automática de duplicatas. ",
        "Foram recuperados ", rv$n_pubmed + rv$n_wos + rv$n_scopus + rv$n_extra,
        " registros, dos quais ", n_dup, " foram removidos como duplicatas. ",
        "O corpus final compreendeu ", n_tot, " registros únicos.",
        if (!is.na(rv$py_median) && rv$n_sem_ano > 0) paste0(" Um total de ", rv$n_sem_ano, " registro(s) sem ano de publicação tiveram o campo imputado com a mediana do corpus (", rv$py_median, "), exclusivamente para fins de cálculo de score.") else "",
        "\n\n",

        "2.3 Análise bibliométrica\n\n",
        "As análises bibliométricas descritivas incluíram produção anual (tendência avaliada pela taxa média aritmética de crescimento anual, AAGR), ",
        "periódicos mais produtivos, países, autores e frequência de palavras-chave. ",
        "Redes de coocorrência e colaboração foram calculadas pelo pacote bibliometrix e clusterizadas pelo algoritmo de Louvain (pacote igraph, seed = ", input$louvain_seed, " para reprodutibilidade). ",
        "As visualizações foram geradas com ggplot2.\n\n",

        "2.4 Priorização e ranqueamento dos artigos\n\n",
        "Cada artigo recebeu um score composto de relevância. ",
        if (usar_eixo) {
          paste0(
            "O score final combinou um componente temático (", pt, "% de peso) e um componente bibliométrico (", pb, "% de peso). ",
            "O componente temático foi baseado em ", nrow(crit), " critérios predefinidos: ", crit_txt, ". ",
            "O score temático foi graduado por campo: uma correspondência no título recebeu o peso integral do critério; ",
            "uma correspondência apenas nas keywords dos autores recebeu 80%; apenas no abstract, 50%.",
            " Termos compostos (múltiplas palavras) foram buscados por correspondência exata; termos simples utilizaram expressões regulares com fronteira de palavra (\\\\b). "
          )
        } else {
          "O score final foi baseado exclusivamente em critérios bibliométricos, sem componente temático. "
        },
        "O componente bibliométrico incorporou ", cit_mode_pt, ", qualidade do periódico e tipo de documento. ",
        rec_pt, " ",
        "Os artigos foram classificados em leitura obrigatória (P1, score ≥ ", lp1, "; n=", p1n,
        "), leitura recomendada (P2, score ", lp2, "–", lp1, "; n=", p2n,
        "), leitura opcional (P3, score ", lp3, "–", lp2, "; n=", p3n,
        "), abaixo do corte (scores entre ", input$limiar_removido, " e ", lp3, "; n=", cutn,
        ") e removidos (P4, score < ", input$limiar_removido, "; n=", p4n, "). ",
        "Artigos classificados como 'abaixo do corte' foram mantidos no corpus mas excluídos da fila de leitura prioritária. ",
        "Todas as análises foram conduzidas em R por meio do LIRA (Literature Integration, Ranking & Analysis, v1.0.0), um pipeline Shiny aberto para triagem e priorização bibliométrica",
        if (nchar(autor) > 0) paste0(", operado por ", autor, if (nchar(inst) > 0) paste0(" (", inst, ")") else "", ".") else ".",
        " Os parâmetros da análise estão documentados na aba 'Configurações da Análise' do material suplementar, garantindo a reprodutibilidade."
      )
    }

    rv$texto_metod <- txt
    showNotification("Texto gerado com sucesso!", type = "message")
  })

  output$texto_metodologia <- renderUI({
    if (nchar(rv$texto_metod) == 0) {
      return(tags$p(
        style = "color:#94A3B8; font-style:italic;",
        "Clique em 'Gerar Texto' para criar o texto de metodologia com base nas configurações atuais."
      ))
    }
    tags$pre(
      style = "white-space:pre-wrap; font-family:Georgia,serif; font-size:12px; line-height:1.7; background:transparent; border:none;",
      rv$texto_metod
    )
  })

  output$dl_metodologia <- downloadHandler(
    filename = function() paste0("metodologia_", format(Sys.Date(), "%Y%m%d"), ".txt"),
    content = function(file) writeLines(rv$texto_metod, file)
  )

  # ── Download Excel ───────────────────────────────────────────
  output$btn_download <- downloadHandler(
    filename = function() paste0(input$export_nome, ".xlsx"),
    content = function(file) {
      df <- classified_df()
      crit <- rv$criterios
      wb <- createWorkbook()

      st_title <- function(bg, sz = 16) {
        createStyle(fontName = "Calibri", fontSize = sz, fontColour = "#FFFFFF",
                    fgFill = bg, halign = "CENTER", valign = "CENTER",
                    textDecoration = "bold")
      }
      st_sub <- function(bg = "#EAF1F8") {
        createStyle(fontName = "Calibri", fontSize = 10, fontColour = "#5D6D7E",
                    fgFill = bg, halign = "CENTER", valign = "CENTER",
                    textDecoration = "italic")
      }
      st_hdr <- function(bg) {
        createStyle(fontName = "Calibri", fontSize = 9, fontColour = "#FFFFFF",
                    fgFill = bg, halign = "CENTER", valign = "CENTER",
                    textDecoration = "bold", wrapText = TRUE,
                    border = "TopBottomLeftRight", borderColour = "#C7D0D9")
      }
      st_dat <- function(bg = "#FFFFFF") {
        createStyle(fontName = "Calibri", fontSize = 8.5, fontColour = "#1F2937",
                    fgFill = bg, halign = "left", valign = "center",
                    border = "TopBottomLeftRight", borderColour = "#E5E7EB",
                    wrapText = TRUE)
      }
      st_num <- function(bg = "#FFFFFF") {
        createStyle(fontName = "Calibri", fontSize = 8.5, fontColour = "#1F2937",
                    fgFill = bg, halign = "CENTER", valign = "center",
                    border = "TopBottomLeftRight", borderColour = "#E5E7EB")
      }
      st_section <- function(bg) {
        createStyle(fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
                    fgFill = bg, halign = "LEFT", valign = "CENTER", textDecoration = "bold")
      }
      st_card_head <- function(bg) {
        createStyle(fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
                    fgFill = bg, halign = "CENTER", valign = "CENTER", textDecoration = "bold")
      }
      st_card_body <- function(bg, font_col = "#1F2937", sz = 26) {
        createStyle(fontName = "Calibri", fontSize = sz, fontColour = font_col,
                    fgFill = bg, halign = "CENTER", valign = "CENTER", textDecoration = "bold")
      }
      st_card_note <- function(bg) {
        createStyle(fontName = "Calibri", fontSize = 10, fontColour = "#5D6D7E",
                    fgFill = bg, halign = "CENTER", valign = "CENTER")
      }

      safe_sheet <- function(x) {
        x <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", x)
        substr(x, 1, 31)
      }

      add_sheet <- function(sname, titulo, sub, data, hcol, wids = NULL) {
        sname <- safe_sheet(sname)
        addWorksheet(wb, sname, gridLines = FALSE)
        nc <- max(1, ncol(data))

        mergeCells(wb, sname, cols = 1:nc, rows = 1)
        writeData(wb, sname, titulo, startRow = 1, startCol = 1)
        addStyle(wb, sname, st_title(hcol, 15), rows = 1, cols = 1)
        setRowHeights(wb, sname, rows = 1, heights = 28)

        mergeCells(wb, sname, cols = 1:nc, rows = 2)
        writeData(wb, sname, sub, startRow = 2, startCol = 1)
        addStyle(wb, sname, st_sub(), rows = 2, cols = 1)
        setRowHeights(wb, sname, rows = 2, heights = 18)

        writeData(wb, sname, data, startRow = 4, startCol = 1, headerStyle = st_hdr(hcol))
        if (nrow(data) > 0) {
          addStyle(wb, sname, st_dat(), rows = 5:(nrow(data) + 4), cols = 1:nc, gridExpand = TRUE)
        }
        freezePane(wb, sname, firstActiveRow = 5, firstActiveCol = 1)
        setColWidths(wb, sname, cols = 1:nc, widths = if (is.null(wids)) "auto" else wids)
      }

      make_tab <- function(priority = NULL) {
        out <- if (!is.null(priority)) df %>% filter(prioridade == priority) else df
        out %>%
          arrange(desc(score_final), desc(score_bibliometrico), desc(TC_num)) %>%
          transmute(
            Autor = clean_str(AU, 35),
            Ano = PY,
            Titulo = clean_str(TI, 110),
            Revista = clean_str(SO, 30),
            Citacoes = TC_num,
            Citacoes_por_ano = citacoes_por_ano,
            Score_Tematico = score_tematico,
            Score_Bibliometrico = score_bibliometrico,
            Score_Final = score_final,
            Prioridade = prioridade,
            Keywords = clean_str(DE, 80),
            DOI = DI
          )
      }

      add_dashboard_sheet <- function() {
        sname <- "Dashboard"
        addWorksheet(wb, sname, gridLines = FALSE)

        mergeCells(wb, sname, cols = 1:12, rows = 1)
        writeData(wb, sname, paste0(safe_theme_title(input$tema_titulo), " — Article Ranking Dashboard"), startRow = 1, startCol = 1)
        addStyle(wb, sname, st_title(COL$navy, 17), rows = 1, cols = 1)
        setRowHeights(wb, sname, rows = 1, heights = 34)

        bases_txt <- paste(c(
          if (rv$n_pubmed > 0) paste0("PubMed (n=", rv$n_pubmed, ")") else NULL,
          if (rv$n_scopus > 0) paste0("Scopus (n=", rv$n_scopus, ")") else NULL,
          if (rv$n_wos > 0) paste0("Web of Science (n=", rv$n_wos, ")") else NULL,
          if (rv$n_extra > 0) paste0(input$extra_dbname, " (n=", rv$n_extra, ")") else NULL
        ), collapse = " + ")
        score_txt <- if (identical(input$usar_eixo_tematico, "sim")) {
          paste0("Score: ", input$peso_tematico, "% temático + ", 100 - input$peso_tematico, "% bibliométrico")
        } else {
          "Score: 100% bibliométrico"
        }

        mergeCells(wb, sname, cols = 1:12, rows = 2)
        writeData(wb, sname, paste0(bases_txt, "  |  Corpus: ", nrow(df), " artigos  |  ", score_txt), startRow = 2, startCol = 1)
        addStyle(wb, sname, st_sub("#EAF1F8"), rows = 2, cols = 1)
        setRowHeights(wb, sname, rows = 2, heights = 22)

        card_cfg <- list(
          list("P1 - OBRIGATORIA", sum(df$prioridade == "P1 - OBRIGATORIA"), paste0("Score ≥ ", input$limiar_p1), 1, 3),
          list("P2 - RECOMENDADA", sum(df$prioridade == "P2 - RECOMENDADA"), paste0("Score ", input$limiar_p2, "–", input$limiar_p1), 4, 6),
          list("P3 - OPCIONAL", sum(df$prioridade == "P3 - OPCIONAL"), paste0("Score ", input$limiar_p3, "–", input$limiar_p2), 7, 9),
          list("P4 - REMOVIDO", sum(df$prioridade == "P4 - REMOVIDO"), paste0("Score < ", input$limiar_removido), 10, 12)
        )

        for (cd in card_cfg) {
          lab <- cd[[1]]
          nval <- cd[[2]]
          sub <- cd[[3]]
          c1 <- cd[[4]]
          c2 <- cd[[5]]
          dk <- priority_dark(lab)
          lt <- priority_light(lab)

          mergeCells(wb, sname, cols = c1:c2, rows = 4)
          writeData(wb, sname, gsub(" - REMOVIDO", " - REMOVIDOS", lab), startRow = 4, startCol = c1)
          addStyle(wb, sname, st_card_head(dk), rows = 4, cols = c1)
          setRowHeights(wb, sname, rows = 4, heights = 24)

          mergeCells(wb, sname, cols = c1:c2, rows = 5:6)
          writeData(wb, sname, nval, startRow = 5, startCol = c1)
          addStyle(wb, sname, st_card_body(lt, dk, 28), rows = 5, cols = c1)
          setRowHeights(wb, sname, rows = 5:6, heights = 32)

          mergeCells(wb, sname, cols = c1:c2, rows = 7)
          writeData(wb, sname, sub, startRow = 7, startCol = c1)
          addStyle(wb, sname, st_card_note(lt), rows = 7, cols = c1)
        }

        mergeCells(wb, sname, cols = 1:12, rows = 9)
        writeData(wb, sname, "INDICADORES BIBLIOMÉTRICOS DO CORPUS", startRow = 9, startCol = 1)
        addStyle(wb, sname, st_section(COL$blue), rows = 9, cols = 1)
        setRowHeights(wb, sname, rows = 9, heights = 22)

        dash_df <- dashboard_indicators()
        writeData(wb, sname, dash_df, startRow = 10, startCol = 1, headerStyle = st_hdr(COL$navy))
        if (nrow(dash_df) > 0) {
          addStyle(wb, sname, st_dat(), rows = 11:(nrow(dash_df) + 10), cols = 1:ncol(dash_df), gridExpand = TRUE)
        }

        net_df <- network_overview()
        row0 <- nrow(dash_df) + 13
        mergeCells(wb, sname, cols = 1:12, rows = row0)
        writeData(wb, sname, "REDES E CLUSTERS — VISÃO ESTRUTURAL", startRow = row0, startCol = 1)
        addStyle(wb, sname, st_section(COL$green), rows = row0, cols = 1)

        writeData(wb, sname, net_df, startRow = row0 + 1, startCol = 1, headerStyle = st_hdr(COL$green))
        if (nrow(net_df) > 0) {
          addStyle(wb, sname, st_dat(), rows = (row0 + 2):(row0 + 1 + nrow(net_df)), cols = 1:ncol(net_df), gridExpand = TRUE)
        }

        kw_cl <- keyword_clusters_tbl()
        row1 <- row0 + nrow(net_df) + 4
        mergeCells(wb, sname, cols = 1:12, rows = row1)
        writeData(wb, sname, "CLUSTERS DE KEYWORDS", startRow = row1, startCol = 1)
        addStyle(wb, sname, st_section(COL$amber), rows = row1, cols = 1)

        writeData(wb, sname, kw_cl, startRow = row1 + 1, startCol = 1, headerStyle = st_hdr(COL$amber))
        if (nrow(kw_cl) > 0) {
          addStyle(wb, sname, st_dat(), rows = (row1 + 2):(row1 + 1 + nrow(kw_cl)), cols = 1:ncol(kw_cl), gridExpand = TRUE)
        }

        freezePane(wb, sname, firstActiveRow = 10, firstActiveCol = 1)
        setColWidths(wb, sname, cols = 1:12, widths = c(22, 18, 42, 14, 12, 12, 12, 12, 12, 12, 12, 12))
      }

      add_queue_sheet <- function() {
        fila <- reading_queue()
        sname <- "Fila_Semanal"
        addWorksheet(wb, sname, gridLines = FALSE)

        mergeCells(wb, sname, cols = 1:10, rows = 1)
        writeData(wb, sname, paste0("FILA DE LEITURA SEMANAL  |  ", nrow(fila), " artigos priorizados"), startRow = 1, startCol = 1)
        addStyle(wb, sname, st_title(COL$navy, 15), rows = 1, cols = 1)

        mergeCells(wb, sname, cols = 1:10, rows = 2)
        writeData(wb, sname, "Ordem otimizada em 6 blocos: Artigos fundadores, obrigatórios recentes, recomendados, opcionais estratégicos e reviews.", startRow = 2, startCol = 1)
        addStyle(wb, sname, st_sub(), rows = 2, cols = 1)

        if (nrow(fila) == 0) {
          writeData(wb, sname, "Nenhum artigo elegível para a fila semanal.", startRow = 4, startCol = 1)
          return(invisible(NULL))
        }

        wk_order <- c(
          "Semana 1 - Artigos fundadores",
          "Semana 2 - Obrigatórios recentes",
          "Semana 3 - Recomendados de maior impacto",
          "Semana 4 - Recomendados complementares",
          "Semana 5 - Opcionais estratégicos",
          "Semana 6 - Reviews e sínteses"
        )

        row_ptr <- 4
        for (wk in wk_order) {
          bloco <- fila %>% filter(Semana == wk)
          if (nrow(bloco) == 0) next

          cor_head <- dplyr::case_when(
            wk == "Semana 1 - Artigos fundadores" ~ "#1E40AF",
            grepl("Obrigatórios", wk) ~ "#1E40AF",
            grepl("Recomendados", wk) ~ "#1E8449",
            grepl("Opcionais", wk) ~ "#C2410C",
            TRUE ~ "#5D6D7E"
          )

          mergeCells(wb, sname, cols = 1:10, rows = row_ptr)
          writeData(wb, sname, wk, startRow = row_ptr, startCol = 1)
          addStyle(wb, sname, st_section(cor_head), rows = row_ptr, cols = 1)
          row_ptr <- row_ptr + 1

          writeData(wb, sname, bloco, startRow = row_ptr, startCol = 1, headerStyle = st_hdr(cor_head))
          if (nrow(bloco) > 0) {
            addStyle(wb, sname, st_dat(), rows = (row_ptr + 1):(row_ptr + nrow(bloco)), cols = 1:ncol(bloco), gridExpand = TRUE)
          }
          row_ptr <- row_ptr + nrow(bloco) + 2
        }

        freezePane(wb, sname, firstActiveRow = 4, firstActiveCol = 1)
        setColWidths(wb, sname, cols = 1:10, widths = c(28, 18, 34, 8, 68, 26, 10, 10, 16, 34))
      }

      add_bibliometria_sheet <- function() {
        sname <- "Bibliometria"
        addWorksheet(wb, sname, gridLines = FALSE)

        mergeCells(wb, sname, cols = 1:8, rows = 1)
        writeData(wb, sname, "BIBLIOMETRIA ROBUSTA — REDES, CLUSTERS E INDICADORES", startRow = 1, startCol = 1)
        addStyle(wb, sname, st_title(COL$navy, 15), rows = 1, cols = 1)

        mergeCells(wb, sname, cols = 1:8, rows = 2)
        writeData(wb, sname, "Resumo quantitativo do corpus, redes bibliométricas e clusters temáticos.", startRow = 2, startCol = 1)
        addStyle(wb, sname, st_sub(), rows = 2, cols = 1)

        annual_df <- rv$df_merged %>%
          mutate(Year = suppressWarnings(as.numeric(PY))) %>%
          filter(!is.na(Year), Year >= input$ano_inicial) %>%
          count(Year, name = "Publicacoes") %>%
          arrange(Year)

        top_inst <- top_institutions()
        kw_cl <- keyword_clusters_tbl()
        net_df <- network_overview()

        mergeCells(wb, sname, cols = 1:8, rows = 4)
        writeData(wb, sname, "PRODUÇÃO ANUAL", startRow = 4, startCol = 1)
        addStyle(wb, sname, st_section(COL$blue), rows = 4, cols = 1)
        writeData(wb, sname, annual_df, startRow = 5, startCol = 1, headerStyle = st_hdr(COL$blue))
        if (nrow(annual_df) > 0) addStyle(wb, sname, st_dat(), rows = 6:(5 + nrow(annual_df)), cols = 1:ncol(annual_df), gridExpand = TRUE)

        rowp <- 7 + nrow(annual_df)
        mergeCells(wb, sname, cols = 1:8, rows = rowp)
        writeData(wb, sname, "MÉTRICAS DE REDE", startRow = rowp, startCol = 1)
        addStyle(wb, sname, st_section(COL$green), rows = rowp, cols = 1)
        writeData(wb, sname, net_df, startRow = rowp + 1, startCol = 1, headerStyle = st_hdr(COL$green))
        if (nrow(net_df) > 0) addStyle(wb, sname, st_dat(), rows = (rowp + 2):(rowp + 1 + nrow(net_df)), cols = 1:ncol(net_df), gridExpand = TRUE)

        rowp <- rowp + nrow(net_df) + 4
        mergeCells(wb, sname, cols = 1:8, rows = rowp)
        writeData(wb, sname, "CLUSTERS DE KEYWORDS", startRow = rowp, startCol = 1)
        addStyle(wb, sname, st_section(COL$amber), rows = rowp, cols = 1)
        writeData(wb, sname, kw_cl, startRow = rowp + 1, startCol = 1, headerStyle = st_hdr(COL$amber))
        if (nrow(kw_cl) > 0) addStyle(wb, sname, st_dat(), rows = (rowp + 2):(rowp + 1 + nrow(kw_cl)), cols = 1:ncol(kw_cl), gridExpand = TRUE)

        rowp <- rowp + nrow(kw_cl) + 4
        mergeCells(wb, sname, cols = 1:8, rows = rowp)
        writeData(wb, sname, "TOP INSTITUIÇÕES", startRow = rowp, startCol = 1)
        addStyle(wb, sname, st_section(COL$p1dk), rows = rowp, cols = 1)
        writeData(wb, sname, top_inst, startRow = rowp + 1, startCol = 1, headerStyle = st_hdr(COL$p1dk))
        if (nrow(top_inst) > 0) addStyle(wb, sname, st_dat(), rows = (rowp + 2):(rowp + 1 + nrow(top_inst)), cols = 1:ncol(top_inst), gridExpand = TRUE)

        freezePane(wb, sname, firstActiveRow = 5, firstActiveCol = 1)
        setColWidths(wb, sname, cols = 1:8, widths = c(20, 16, 16, 14, 14, 14, 24, 58))
      }

      if ("dashboard" %in% input$export_sheets) add_dashboard_sheet()
      if ("p1" %in% input$export_sheets) add_sheet("P1", "P1 — Leitura Obrigatória", "Artigos com maior prioridade", make_tab("P1 - OBRIGATORIA"), COL$p1dk)
      if ("p2" %in% input$export_sheets) add_sheet("P2", "P2 — Leitura Recomendada", "Artigos recomendados para aprofundamento", make_tab("P2 - RECOMENDADA"), COL$p2dk)
      if ("p3" %in% input$export_sheets) add_sheet("P3", "P3 — Leitura Opcional", "Artigos úteis para expansão do corpus", make_tab("P3 - OPCIONAL"), COL$p3dk)
      if ("fila" %in% input$export_sheets) add_queue_sheet()
      if ("bibliometria" %in% input$export_sheets) add_bibliometria_sheet()
      if ("abaixo" %in% input$export_sheets) add_sheet("Abaixo_Corte", "ABAIXO DO CORTE", "Artigos mantidos, mas abaixo do limiar de P3", make_tab("ABAIXO DO CORTE"), "#E67E22")
      if ("p4" %in% input$export_sheets) add_sheet("P4", "P4 — Removidos", "Artigos abaixo do limiar de remoção", make_tab("P4 - REMOVIDO"), COL$p4dk)
      if ("ranking" %in% input$export_sheets) add_sheet("Ranking", "RANKING COMPLETO", "Todos os artigos com score detalhado", make_tab(), COL$navy)

      if ("prisma" %in% input$export_sheets) {
        prisma_df <- data.frame(
          Etapa = c(
            "PubMed","Web of Science","Scopus","Extra","TOTAL BRUTO",
            "Duplicatas Removidas","CORPUS FINAL","P1 Obrigatória",
            "P2 Recomendada","P3 Opcional","Abaixo do corte","P4 Removidos",
            "Para leitura (P1+P2)","Incluídos no review (preencher)"
          ),
          N = c(
            rv$n_pubmed, rv$n_wos, rv$n_scopus, rv$n_extra,
            rv$n_pubmed + rv$n_wos + rv$n_scopus + rv$n_extra,
            rv$dup_rem, nrow(df),
            sum(df$prioridade == "P1 - OBRIGATORIA"),
            sum(df$prioridade == "P2 - RECOMENDADA"),
            sum(df$prioridade == "P3 - OPCIONAL"),
            sum(df$prioridade == "ABAIXO DO CORTE"),
            sum(df$prioridade == "P4 - REMOVIDO"),
            sum(df$prioridade %in% c("P1 - OBRIGATORIA", "P2 - RECOMENDADA")),
            NA
          )
        )
        add_sheet("PRISMA", "PRISMA FLOW — Números para o Manuscrito", "Atualize a linha final após a leitura completa", prisma_df, COL$navy, wids = c(38, 12))
      }

      if ("template" %in% input$export_sheets) {
        tmpl <- data.frame(
          ID = integer(), Autor_Ano = character(), Titulo = character(),
          DOI = character(), Revista = character(), Ano = integer(),
          Citacoes = integer(), Score = numeric(), Prioridade = character(),
          Tipo_Estudo = character(), Objetivo = character(),
          Populacao_Amostra = character(), Metodos = character(),
          Principais_Resultados = character(), Principal_Achado = character(),
          Limitacoes = character(), Gap_para_Review = character(),
          Relevancia = character(), Secao_Manuscrito = character(), Notas = character()
        )
        add_sheet("Template", "TEMPLATE DE ANOTAÇÃO", "Preencha um registro por artigo lido", tmpl, COL$blue)
      }

      if ("config" %in% input$export_sheets) {
        cfg_param <- c(
          "Tema","Autor","Instituição","Data da busca","Ano inicial",
          "Bases utilizadas","String de busca",
          "Usar eixo temático","Modo do eixo temático","Peso temático (%)","Peso bibliométrico (%)",
          "Método de score temático",
          "Usar score de revistas","Tier 1 — revistas alto impacto (score 5)","Tier 2 — revistas médio impacto (score 4)",
          "Tier 3 — revistas impacto moderado (score 3)","Tier 4 — revistas impacto básico (score 2)",
          "Modo citações","Usar recência","Método de crescimento anual","Seed Louvain",
          "Limiar P1","Limiar P2","Limiar P3","Limiar removidos"
        )
        cfg_val <- c(
          input$tema_titulo, input$autor_nome, input$instituicao,
          format(input$data_busca, "%d/%m/%Y"), input$ano_inicial,
          paste(c(
            if (rv$n_pubmed > 0) "PubMed" else NULL,
            if (rv$n_wos > 0) "WoS" else NULL,
            if (rv$n_scopus > 0) "Scopus" else NULL,
            if (rv$n_extra > 0) input$extra_dbname else NULL
          ), collapse = " + "),
          input$string_busca,
          if (identical(input$usar_eixo_tematico, "sim")) "Sim" else "Não",
          if (identical(input$usar_eixo_tematico, "sim")) input$modo_eixo_tematico else "Não se aplica",
          if (identical(input$usar_eixo_tematico, "sim")) input$peso_tematico else 0,
          if (identical(input$usar_eixo_tematico, "sim")) 100 - input$peso_tematico else 100,
          "Graduado por campo: TI=100%, DE=80%, AB=50%; termos multi-palavra: busca exata; single-word: word-boundary regex",
          if (isTRUE(input$usar_score_revistas)) "Sim" else "Não",
          if (isTRUE(input$usar_score_revistas)) input$revistas_top   else "N/A",
          if (isTRUE(input$usar_score_revistas)) input$revistas_med   else "N/A",
          if (isTRUE(input$usar_score_revistas)) input$revistas_tier3 else "N/A",
          if (isTRUE(input$usar_score_revistas)) input$revistas_tier4 else "N/A",
          if (identical(input$modo_citacoes_tempo, "ponderado")) "Ponderado por citações/ano" else "Bruto",
          if (isTRUE(input$usar_recencia)) "Sim" else "Não",
          "AAGR (média aritmética das taxas anuais de crescimento)",
          as.character(input$louvain_seed),
          input$limiar_p1, input$limiar_p2, input$limiar_p3, input$limiar_removido
        )

        if (nrow(crit) > 0) {
          cfg_param <- c(cfg_param, paste0("Critério ", seq_len(nrow(crit)), " — ", crit$nome))
          cfg_val <- c(cfg_val, crit$keywords)
        }

        cfg_df <- data.frame(Parametro = cfg_param, Valor = cfg_val, stringsAsFactors = FALSE)
        add_sheet("Configuracoes", "CONFIGURAÇÕES DA ANÁLISE", "Parâmetros necessários para reprodução", cfg_df, COL$teal, wids = c(40, 110))
      }

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # ── Downloads figuras ────────────────────────────────────────
  fig_dl <- function(pn, fn) {
    downloadHandler(
      filename = function() fn,
      content = function(file) {
        req(rv$plots[[pn]])
        ggsave(file, rv$plots[[pn]], dpi = 300, width = 10, height = 6, bg = "white")
      }
    )
  }

  output$dl_fig1 <- fig_dl("annual", "fig1_producao_anual.png")
  output$dl_fig2 <- fig_dl("journals", "fig2_top_revistas.png")
  output$dl_fig3 <- fig_dl("countries", "fig3_top_paises.png")
  output$dl_fig4 <- fig_dl("authors", "fig4_top_autores.png")
  output$dl_fig5 <- fig_dl("keywords", "fig5_keywords.png")
}

shinyApp(ui, server)
