# LIRA — Literature Integration, Ranking & Analysis

> Bilingual (PT/EN) Shiny application for multi-database bibliometric analysis, article prioritization, and reading queue generation.

## Description

A fully interactive R/Shiny pipeline for systematic and scoping reviews with a complete bilingual interface (Portuguese ↔ English, switchable at runtime). Supports PubMed, Web of Science, Scopus, and custom databases. Performs automatic deduplication, composite relevance scoring (thematic + bibliometric components), network analysis (keyword co-occurrence, author and country collaboration), and structured Excel export with PRISMA flow data.

Developed in the context of multi-omics research at the Núcleo de Genética Humana e Molecular (NGHM), Universidade Federal do Espírito Santo (UFES).

## Features

- **Bilingual interface** — full PT/EN UI with instant switching, persistent across sessions
- Import from PubMed (.txt/.nbib), Web of Science (.txt), Scopus (.bib/.ris/.csv), and custom sources
- Automatic deduplication via the `bibliometrix` package
- Composite scoring: thematic component (field-weighted keyword match on title, keywords, abstract) + bibliometric component (citations, recency, journal tier, document type)
- Priority classification: P1 (mandatory), P2 (recommended), P3 (optional), P4 (removed)
- Structured 6-week reading queue
- Bibliometric networks with Louvain clustering (igraph); reproducible seed exposed in UI
- Recency scoring relative to search date (not hardcoded years)
- Cross-system file navigation: automatic detection of all available drives
- Export: styled multi-sheet .xlsx with PRISMA flow, annotation template, network metrics, and auto-generated methods text in PT and EN
- Figure download (PNG 300 DPI)
- Editorial-grade UI: Fraunces (display) + Plus Jakarta Sans (UI) + JetBrains Mono (numbers)

## Requirements

- R ≥ 4.2
- Packages: `shiny`, `shinydashboard`, `shinyFiles`, `shinyWidgets`, `bibliometrix`, `tidyverse`, `openxlsx`, `ggplot2`, `DT`, `waiter`, `igraph`

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyFiles", "shinyWidgets",
  "bibliometrix", "tidyverse", "openxlsx", "ggplot2",
  "DT", "waiter", "igraph"
))
```

## Usage

```r
shiny::runApp("lira.R")
```

## Citation

If you use this software in a publication, please cite:

> Basílio, L.A., Casotti, M.C., Meira, D.D., Louro, I.D. (2025). *LIRA: Literature Integration, Ranking & Analysis* (v1.0.0). Universidade Federal do Espírito Santo (UFES), Vitória, ES, Brasil. https://doi.org/[DOI]

## Authors

**Leandro Araújo Basílio** *(author, maintainer)*  
Mestrando em Biotecnologia — UFES  
Núcleo de Genética Humana e Molecular (NGHM)  
Vitória, ES, Brasil

**Matheus Correia Casotti** *(author — conceptualization, testing, validation)*  
NGHM — Universidade Federal do Espírito Santo  
Vitória, ES, Brasil

**Débora Dummer Meira** *(author, thesis advisor)*  
Professora — Departamento de Ciências Biológicas, UFES  
Núcleo de Genética Humana e Molecular (NGHM)  
Vitória, ES, Brasil

**Iuri Drumond Louro** *(author)*  
NGHM — Universidade Federal do Espírito Santo  
Vitória, ES, Brasil

## Collaborator

**Roquemar de Lima Baldam** *(contributor)*  
Universidade Federal do Espírito Santo  
Vitória, ES, Brasil

## License

Copyright © 2025 Leandro Araújo Basílio. All rights reserved.  
See [LICENSE](LICENSE) for details.
