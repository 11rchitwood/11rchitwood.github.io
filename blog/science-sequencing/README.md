# Georgia high school science sequencing

This project compares the six possible orders of Georgia’s Biology I, Chemistry I, and Physics using a directed network of conceptual links. The initial dependency inventory is an AI-assisted research draft. All 122 judgments are provisional pending human review.

## Reproduce the draft

From this directory, install the R packages if needed and run the analysis. The saved standards and dependency CSVs make analysis and rendering independent of internet access.

```r
install.packages(c("jsonlite", "readr", "dplyr", "tidyr", "purrr", "tibble",
                   "igraph", "ggplot2", "scales", "svglite", "knitr", "rmarkdown"))
```

```sh
Rscript R/export.R
quarto render index.qmd --metadata draft:false
```

The command-line metadata override makes this draft visible in the local output. The checked-in Quarto document retains its draft setting. The figures in `figures/` are vector SVGs. For inspectable interim tables, use `Rscript R/export.R figures /path/to/analysis-tables`. The export includes each edge’s satisfaction for each order, course-pair counts and densities, node degrees, reciprocal pairs and cycles in the positive-weight network, sensitivity scores, simulation results, and R session information. The initial run used Quarto 1.9.35 and R 4.5.3.

## Standards provenance

- `data/standards.csv` has one row per official lettered CASE element, with its parent ID and parent wording as context. The element's wording comes from `fullStatement`, including bullets and newlines. Its source is the item's CASE URI. The catalog timestamp in `source_version` identifies the framework snapshot. `item_last_modified` tracks the digital record for that specific element. Neither timestamp establishes a new effective date.
- GaDOE lists the framework as adopted. On November 27, 2026, the [CASE document directory](https://case.georgiastandards.org/ims/case/v1p1/CFDocuments) listed one adopted general science framework, last modified May 29, 2025. The selected course codes are Biology I 26.01200 (24 elements), Chemistry I 40.05100 (34), and Physics 40.08100 (28).
- The [SuitCASE framework](https://case.georgiastandards.org/27a08dc6-416e-11e7-ba71-02bd89fdd987/27a08dc6-416e-11e7-ba71-02bd89fdd987) notes a June 9, 2016 approval, and the [GaDOE press release](https://apps.gadoe.org/Lists/GaDOE%20Press%20Releases/ViewPressReleases.aspx?ID=424) announced approval on March 31, 2016. Both give the 2017–18 school year as first implementation, as does the [GaDOE FAQ](https://inspire.gadoe.org/user-files/9391da0f-5dd6-4e64-8075-9bab16f9cf44.pdf). The catalog's 2019 adoption note covers other high school science courses.
- Run `Rscript R/acquire.R` to refresh from the [official CASE API](https://case.georgiastandards.org/ims/case/v1p1/CFPackages/27a08dc6-416e-11e7-ba71-02bd89fdd987). Pass a saved CASE JSON as the first argument to reproduce a specific extraction. Compare the resulting diff, especially any edits to objectives referenced in the dependency CSV.
- The standards are attributed to the Georgia Department of Education. The CSV repackages its official text and adds provenance columns under the framework’s [CC BY-NC-SA terms](https://case.georgiastandards.org/uri/47df204e-0584-4a22-b8d7-2ff0b686d989).

## Dependency classification rubric

The initial screen started with what students are asked to do in the receiving objective. For each of the six cross-course directions and within each discipline, it compared the process, mechanism, model, or representation needed by the receiver with concepts taught by potential sources. It retained select candidate pairs with a specific use in the target. No part of the analysis turns generic CASE associations, shared terms, examples, or transitive paths into automatic links. The screen yields an inventory of proposed links. All unrecorded pairs remain open to review.

1. **Identify the knowledge transferred.** Read both elements with their parent statements. Name the specific idea taught by the source and the aspect of the target performance it supports. Check the direction independently of the reverse. The relevant idea may be only one part of a compound objective.
2. **Test the target at its specified depth.** Ask what a student could explain, model, calculate, or investigate with that idea absent, assuming ordinary instruction in the target course. Use the definitions below. A concept can be required without requiring the entire prerequisite course. A deeper real-world mechanism alone does not warrant a required or helpful link.
3. **Check alternatives and overlap.** If the target can use a completely different example, record reinforcement only if there is a concrete way to revisit the source idea. Omit links based solely on a shared word, a generic science practice, an analogy, or indirect scientific ancestry. Record both directions only with distinct rationales. Keep one row per directed pair and choose the strongest defensible relationship when several aspects of that pair connect.
4. **Assign confidence separately from strength.** Use `high` when both texts clearly support the proposed transfer and scope, `medium` when one end requires a modest inference, and `low` when the transfer depends on an optional example or a substantial interpretation. These labels reflect review priority and are ordinal judgments.

| Type | Decision rule | Default weight |
|---|---|---:|
| `required` | Without the concept taught by the source, a meaningful account of a specified target task is incomplete. | 3 |
| `helpful` | The concept materially improves performance at the specified level. A reasonable course or unit can introduce the concept or meet the expectation through another route. | 2 |
| `enriching` | The concept supports a deeper mechanism or optional application beyond the target's minimum expectation. | 1 |
| `reinforcing` | The target can revisit or apply a source idea, with no expected penalty for meeting the target when the idea is taught at the point of use. | 0 |

Examples from the initial screen:

| Direction | Decision and rationale |
|---|---|
| Chemistry → Biology | SC2.d → SB1.d is helpful. Bond polarity helps explain why a membrane selectively permits certain substances, even though a biology unit can introduce polarity. |
| Biology → Chemistry | SB1.c → SC4.c is reinforcing. Enzymes can serve as an example of catalysis. The chemistry objective can use a different catalyst. |
| Physics → Chemistry | SP4.e → SC1.e is helpful because experience interpreting spectral composition supports reasoning with element-specific emission lines. |
| Chemistry → Physics | SC1.b → SP6.b is helpful because proton count helps explain why some decay types change the element's identity. |
| Physics → Biology | SP4.e → SB1.e is enriching when students elect to compare different colors of illumination in a photosynthesis inquiry. |
| Biology → Physics | No links met the rubric in this pass. Knowing an organelle's function can supply context for viewing cells, but does not explain how lenses focus light (SP4.f). |
| Within Biology | SB2.a → SB2.b is required for the part of the argument where students explain the origin of variation from errors during replication. |

## Maintain the edge review

Edit `data/dependencies.csv` independently of the analysis. Keep the required columns and use UTF-8.

| Column | Contents |
|---|---|
| `prerequisite_id`, `dependent_id` | Exact `substandard_id` values, in the direction in which knowledge transfers. |
| `dependency_type`, `weight` | A category from the rubric and its default weight. The baseline uses the CSV's weights, which can be adjusted while staying positive for prerequisites and zero for reinforcement. |
| `rationale` | The aspect of the target task that draws on the source concept, and why the assigned strength fits. |
| `confidence` | `high`, `medium`, or `low`. |
| `review_status` | `provisional` for an unadjudicated candidate, `accepted` for human-confirmed coding, and `rejected` for a preserved decision excluded from analysis. |
| `concept` | A stable topic label such as `Molecular interactions`. Reuse it for links involving the same topic because the concept sensitivity checks group on it. |

For classroom observations, record the target ID, the task and student reasoning, the concept they appeared to need, what had already been taught, and any bridge lesson that helped. Compare that observation with the exact standard and ask a second reviewer to code the pair and direction independently. Keep observation notes and reviewer/date in additional CSV columns if helpful, using de-identified classroom evidence. Track disagreements and evidence for rejected links in the rationale or optional notes. A later version could use independent screeners to check candidate omissions as well as the classification of existing edges.

## Scoring and sensitivity

The baseline includes accepted and provisional links and gives each positive cross-course edge its stored weight. All three score denominators are the number or weight of eligible cross-course edges for the relevant metric. A zero denominator yields an undefined score. The unsatisfied totals are grouped by dependent course and strength. Counts per 100 possible directed pairs use `n_source × n_target` across courses and `n × (n − 1)` within a course. The course-normalized scenario gives each receiving course with eligible links equal total weight. The pair-normalized scenario divides each edge weight by the possible pair count in its direction.

The granularity checks collapse to the strongest edge per parent-standard pair and to the strongest edge per topic in each direction. A separate check removes each cross-course topic in turn. These checks examine repeated links and coarser coding. A finer atomization would require mapping each incoming and outgoing rationale to the relevant clause and recoding the edges.

The simulation uses 2,000 draws and seed 2718. A provisional positive edge moves one category up or down with equal chances conditional on perturbation. Perturbation probabilities are 10%, 30%, and 60% for high, medium, and low confidence. Changes are capped at required or zero. Accepted edges are held fixed, as are zero-weight reinforcing links, and uncoded pairs are outside the simulation. The assumptions are illustrative. Simulation frequencies do not quantify real reviewer disagreement or student outcomes. When revising the study, review the draft’s narrative, examples, and figure descriptions alongside the newly computed tables and scores.
