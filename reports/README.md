# Raport trendów sprzedaży

Plik `raport_trendow_sprzedazy.Rmd` generuje raport HTML obejmujący:

- trendy sprzedaży (sklepy, kategorie),
- skuteczność promocji,
- porównanie sklepów i kategorii,
- prognozę Prophet,
- opcjonalną analizę segmentu.

## Wymagania

- Dane CSV w folderze `data/` (jak w głównym README pakietu)
- Pakiet `analitickit` zainstalowany lub `devtools::load_all()`
- Do renderowania: `rmarkdown`, `knitr`
- Do prognozy: `prophet` (opcjonalnie — bez niego sekcja prognozy jest pomijana)

```r
install.packages(c("rmarkdown", "knitr", "prophet"))
devtools::install()  # lub devtools::load_all() z katalogu pakietu
```

## Renderowanie

Po zmianach w kodzie pakietu uruchom `devtools::load_all()` albo przeinstaluj pakiet.
Raport z repozytorium ładuje automatycznie kod źródłowy (`devtools::load_all`),
jeśli renderujesz z katalogu `reports/` lub głównego.

Z katalogu głównego repozytorium:

```r
rmarkdown::render(
  "reports/raport_trendow_sprzedazy.Rmd",
  params = list(
    data_path = "data",
    forecast_store = 1,
    forecast_family = "AUTOMOTIVE",
    forecast_horizon = 30
  )
)
```

Z linii poleceń:

```bash
Rscript -e "rmarkdown::render('reports/raport_trendow_sprzedazy.Rmd')"
```

Wynik: `reports/raport_trendow_sprzedazy.html`

## Parametry raportu

| Parametr | Opis | Domyślnie |
|----------|------|-----------|
| `data_path` | Ścieżka do folderu z CSV | `"data"` |
| `forecast_store` | Sklep do prognozy | `1` |
| `forecast_family` | Kategoria do prognozy | `"AUTOMOTIVE"` |
| `forecast_horizon` | Horyzont prognozy | `30` |
| `forecast_period` | `"day"`, `"week"`, `"month"` | `"day"` |
| `segment_city` | Filtr miasta (opcjonalny) | `null` |
| `segment_store_type` | Filtr typu sklepu | `null` |
| `segment_start_date` | Data początkowa segmentu | `null` |
| `segment_end_date` | Data końcowa segmentu | `null` |

Przykład z segmentem (Quito, typ D, 2016):

```r
rmarkdown::render(
  "reports/raport_trendow_sprzedazy.Rmd",
  params = list(
    segment_city = "Quito",
    segment_store_type = "D",
    segment_start_date = "2016-01-01",
    segment_end_date = "2017-08-15"
  )
)
```
