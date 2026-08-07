---
editor_options: 
  markdown: 
    wrap: 72
---

# mzXplorer Instructions

*mzXplorer* is an interactive Shiny application for exploring
high‑resolution MS data using **mass‑defect visualisation**, **RT‑ and
CCS‑aware homologue‑series detection**, and **in‑source fragmentation
(ISF) annotation**.

It is designed for LC/GC‑HRMS, DI/FIA, DIA, and **IM‑MS** workflows.

------------------------------------------------------------------------

# 1. Input Data

### 1.1 Required columns

mzXplorer accepts a single **CSV (.csv) or Excel (.xlsx / .xls)** feature file with at least:

| Column      | Description                 |
|-------------|-----------------------------|
| `mz`        | Mass‑to‑charge ratio        |
| `rt`        | Retention time (min or sec) |
| `intensity` | Peak height or area         |

-   `rt` may be constant (e.g. direct infusion).
-   Column names are automatically lower‑cased and trimmed.
Names must follow standard R variable rules (letters, digits, `_`, `.`).

#### 1.1.1 Optional columns

Any additional columns are allowed and become available for plotting,
filtering, homologue summary and export. Examples: `ccs`, `class`,
`group`, `chemical_formula`, `sample_id`, `feature_id`, per‑sample
intensity columns (`sample_1`, `sample_2`, …).

#### 1.1.2 Column mapping 

After a feature file is uploaded, the **Column mapping** section lets you
assign the file's actual column names to the roles mzXplorer expects
(`mz`, `rt`, `intensity`, and optionally `ccs` and `id`). Reasonable
defaults are guessed automatically — just confirm and press **Process**.


------------------------------------------------------------------------

::::: {style="display:grid; grid-template-columns: 1fr 1fr; gap: 2rem; align-items: start;"}
<div>

# 2. Mass Defect (MD) tab

The MD tab is for exploring feature lists via mass‑defect plots,
detecting homologue series, computing MD / mz differences, and
comparing samples.

### 2.1 Workflow

1.  **Upload feature list file** → confirm column mapping.
2.  **Mass Defect formulas** → enter one to **three mass‑defect bases** entered as:

| Input      | Meaning                      |
|------------|------------------------------|
| `CH2`      | CH₂ mass                     |
| `Cl-H`     | replacement of H with Cl     |
| `CH2,O`    | first = CH₂, second = O      |
| `CH2,Cl-H` | CH₂ and (Cl−H)               |
| `CH2/10`   | fractional mass with base 10 |

3.  **Rounding mode** → choose how nominal masses are computed inside the MD formulas:

| Option     | Meaning                                              |
|------------|------------------------------------------------------|
| `round`    | standard rounding to the nearest integer (default)   |
| `ceiling`  | always round up to the next integer                  |
| `floor`    | always round down to the previous integer            |
The same mode is applied consistently to MD1, MD2, and MD3.

\
After clicking **Process** it computes:  
-   **OMD** – original mass defect
-   **RMD** – relative mass defect
-   **MD1 (MD2**, **MD3)** – user input mass defects\
  All appear in axis selectors for the interactive plots.
  
3.  Choose plot ranges (intensity, mz, rt) and click **Plot**. 
    **Two synchronized scatter plots** appear using Plotly.
4.  Optional sections below are toggled by checkboxes in the sidebar:
    *Homologue search*, *MD / m/z differences calculation*, *Sample comparison*.

### 2.2 Interactive plots

-   Two Plotly scatter plots side-by-side. Each plot has independent X variable and Y Variabel. Any numeric column is selectable.
-   Enabling **Show intensity as size** scales point size by selected intensity column.
-   **Lasso / box select** in either scatter or the MD/mz **network
    plot** highlights the same features in all three views
    (crosstalk‑linked).
-   Selection drives the **selected‑data table**, the **barplot**, the
    **MD/mz difference table**, and the **sample comparison plot**.
-   Use the **Reset selection** button on the tab to clear all linked
    selections and re‑render the plots.

### 2.3 Filters

Three automatically‑generated filters:

-   intensity range
-   m/z range
-   retention time (rt) range

Filtering affects plots, homologue detection, the summary table, and
exported data. Click **Plot** to apply.

### 2.4 Homologue search *(optional)*

mzXplorer implements a graph‑based homologue finder with **RT** and **CCS** monotonicity.
Enable **Detect homologues in filtered data** to reveal parameters:
repeating unit, ppm tolerance, minimum length, RT tolerance, RT trend,
gap allowance, spline R², and optional CCS rules. Click **Calculate
homologues** to run. Results appear in the *Homologue series*
section.

 Parameter | Description |
|----|----|
| Repeating unit | formula (e.g. CH2, CF2, or C2H4O) |
| ppm tolerance | mass error in ppm |
| Minimum length | least number of homologues to be considered a series |
| RT tolerance | max rt difference per step |
| RT trend | increasing / decreasing / any |
| Allow gaps | allows k = 2 jumps (allow skipping one repeating unit in the middle of a series) |
| Spline R² | RT smoothness filter |
| Enable CCS/ ion mobility rules | activates CCS/ion mobility settings. Variable used is selected in Column Mapping |
| CCS mode | RT only / CCS only / Both |
| CCS tolerance | max CCS difference per step |

##### The algorithm performs:

1.  **Candidate edges** via mass defect + optional RT restrictions (`build_edges()`).
2.  **Graph construction** (`build_graph()`).
3.  **Connected components** → provisional series.
4.  **Monotonicity filtering** (`strict_rt_filter()`):
    -   RT monotonicity\
    -   CCS monotonicity\
    -   CCS tolerance\
5.  **Minimum length** check.\
6.  **Chromatographic smoothness** (`apply_shiny_splines()`).
7.  Final renumbering of surviving series.
_____________________________________________________________________

##### CCS‑Aware Monotonicity

When CCS support is enabled, the user may choose:

**RT only**

-   classical RT monotonicity
-   CCS ignored

**CCS only**

-   CCS monotonicity enforced\
-   RT ignored\
-   optional CCS tolerance filter

**Both RT + CCS**

A point must satisfy:

-   RT monotonicity *and*\
-   CCS monotonicity *and*, if enabled\
-   CCS tolerance per step

Ideal for LC‑IM‑HRMS workflows.
_____________________________________________________________________

Homologue table list all series passing the filters and includes:

-   `series_id`, `n`\
-   `mz_min`, `mz_max`\
-   `rt_min`, `rt_max`\
-   `int_sum`
-   `ccs_min`, `ccs_max`, `ccs_range` (if CCS is present)

##### Bar plot
Barplot shows x = m/z, y = relative (% of max) of selected variable
Only selected points appear. Updates when selection changes (plot ↔ homologue table) or intensity variable changes


Clicking one or more rows in the homologue table:

-   colours those series in both interactive plots,
-   dims other points,
-   filters the barplot / selected‑data table to that series (series
    selection overrides plot selection).

Use **Clear series selection** to return to plot‑based selection.


### 2.5 MD / m/z differences *(optional)*

Enable **Calculate Mass Defect Differences** to enter comma‑separated MD
(mDa) and m/z (Da) pairs plus an m/z tolerance. Click **Calculate MD/mz
Differences** to add `Δ → id` columns to the feature table and build a
difference **network plot**.

-   Network plot display features matching the selected MD and mz difference.
-   The results table is **filtered by the network plot's selection**
    (lasso/box). Clear the selection to see all features.

### 2.6 Sample comparison *(optional)*

Enable **Enable sample comparison** to be able to compare selected features (from other plots) accross different samples.
Choose the number of samples and selected the samples intesnity columns.
Plot types: **Grouped bars** or **Lines + markers**.

-   Plot is drawn on **Plot sample comparison**; settings apply on
    click.
-   Any subsequent selection in the scatter or network plots
    auto‑refreshes the sample plot (capped at 40 features for
    legibility).
-   Legend format:
    `id=… | mz=… | rt=… | intensity=…`.

### 2.7 Selection logic

The MD tab uses **unified selection priority**, backed by a
`reactiveVal` + generation counter so **Reset selection**  clears
all linked plots and tables.

#### Priority:
1.  **Homologue‑table selection** (highest) — drives selected‑data
    table, barplot and colour overlays.
2.  **Plot / network selection** — active when no series is selected;
    feeds selected‑data table, MD/mz difference table, and sample
    comparison.
    

### 2.8 Export

**Export Data** downloads selected rows if any exist, otherwise the full
filtered dataset. Includes all MD variables, homologue IDs, and CCS
values if present.

### 2.9 Mass Defect Equations

In all equations below, $\text{nom}(\cdot)$ denotes the **nominal-mass operator** and can be chosen as `round`, `ceiling`, or `floor`:


The same operator is applied to every occurrence of $\text{round}(\cdot)$ shown below.

<p><b>OMD</b> — original mass defect</p>

<p>$$ \text{OMD} = \text{round}(m) - m $$</p>

<p><b>RMD</b> — relative mass defect</p>

<p>$$ \text{RMD} = \frac{\text{round}(m) - m}{m} \times 10^{6} $$</p>

<p><b>MD1</b> — first‑order MD with unit $u_1$</p>

<p>$$ m_1 = m \times \frac{\text{round}(u_1)}{u_1} $$</p>

<p>$$ \text{MD1} = \text{nom}(m_1) - m_1 $$</p>

<p><b>MD2</b> — second‑order MD with unit $u_2$</p>

<p>$$ m_2 = \frac{\text{MD1}(m)}{\text{MD1}(u_2)} $$</p>

<p>$$ \text{MD2} = \text{nom}(m_2) - m_2 $$</p>

<p><b>MD3</b> — third‑order MD with unit $u_3$</p>

<p>$$ m_3 = \frac{\text{MD2}(m)}{\text{MD2}(u_3)} $$</p>

<p>$$ \text{MD3} = \text{nom}(m_3) - m_3 $$</p>

**Examples of the MD‑formula input box**

-   `CH2` — one MD base unit (methylene, exact mass 14.01565).
-   `Cl-H` — addition of one Cl atom and subtraction of one H atom
    (exact mass 33.96103). Use the minus sign `-` to separate the base
    units; only two units per order are supported.
-   `CH2,O` — `CH2` is the first‑order MD unit and `O` is the
    second‑order MD unit. Comma without blank space separates them. Up
    to three orders are supported.
-   `CH2,Cl-H` — `CH2` is first‑order and `Cl-H` is second‑order.
-   `CH2/X` — **fractional base unit** with divisor `X`. Not combinable
    with subtraction. Example: `CH2/10`.

### 2.10 Tips

-   **LC/GC‑HRMS**: RT trend = *increasing*; enable *Both (RT + CCS)*;
    spline R² \> 0.95 for a stricter monotonic trend.
-   **IM‑MS**: activate CCS mode; tune CCS tolerance.
-   **PFAS**: use `CF2` or `CF2,SO2` as the MD unit.
-   **Lipids**: use `CH2`; allow gaps for missing chain lengths.
-   **Direct infusion**: RT trend = *any*; spline R² = 0.
-   Start with a wide **MD/mz difference tolerance** and tighten once
    you see which pairs actually link.

</div>

<div>

# 3. In‑Source Fragmentation (ISF) tab

The ISF tab appears when the sidebar checkbox **"Show ISF analysis tab"** (in the Mass Defect tab) is ticked. It annotates MS1 features that are likely in-source fragments
of other features, using an MS2 fragment file. It shares the same
crosstalk selection model, sample comparison, and column‑mapping as the MD tab.

### 3.1 Workflow

1.  **Upload feature list file** → confirm column mapping.
2.  **Upload fragment file** → Only required for In-source fragmentation annotation. 
Supports both .mgf and .msp format. 
Should contain the fragments of the feature as it used to find matches between fragment ions (MS2) and feature ions (MS1). 
Needs to contain fragment peaks, retention time and precursor mz.  

3.   Set **m/z tolerance (Da)** (default 0.01 Da) and **RT tolerance**
     for detecting ISF, and optionally add **neutral losses** and
     **adducts** to annotate (see 3.7). Press **Process**.
4.  Click **Plot**. mzXplorer produces:
    -   two synchronized scatter plots,
    -   an **ISF network plot**,
    -   an **ISF ratio plot** (fragment/precursor intensity vs. RT),
    -   a **selection‑linked feature table**,
    -   a **processed feature table** in the yellow export section.

### 3.2 Interactive plots

-   Two Plotly scatter plots side-by-side. Each plot has independent X variable and Y variable. Any numeric column is selectable.
-   Enabling **Show intensity as size** scales point size by the selected intensity column.
-   **Lasso / box select** in any of the scatter plots, the **ISF
    network plot** or the **ISF ratio plot** highlights the same
    features across all views.
-   The **ISF network plot** uses 5 categories with distinct colours:
    *Precursor / clean* (blue), *ISF (fragment)* (orange),
    *ISF (neutral loss)* (green), *ISF (fragment + NL)* (purple) and
    *Adduct* (pink). Adduct nodes have no edges.
-   The **ISF ratio plot** shows only ISF pairs (adducts excluded) with
    3 legend entries: *Fragment*, *Neutral loss* and
    *Fragment + Neutral loss*. Selected points from any linked plot are
    over‑drawn in the amber highlight colour.
-   Use the **Reset selection** button on the tab to clear all linked
    selections and re-render the plots.

### 3.3 Feature table (selection‑linked)

Selection behaviour:

-   When a selection is made in **any plot**, the selection‑linked
    table is filtered to show ONLY the selected features (all rows are
    pre‑selected).
-   When a selection is made **in the table** (row clicks), the table
    keeps showing ALL features and the selection is echoed to every
    plot as highlighted points — the table is NOT filtered.
-   When nothing is selected, the table shows all features.

Columns:
-   `ISF_dDa` = difference between m/z of MS2 fragment and ISF features in Da.
-   `ISF_NL_dDa` = difference between feature-to-feature gap and user-input neutral 
    loss mass, in Da. For example a measured 18.0120 Da gap between two features vs H₂O (18.0106) yields `0.0014`.
-   `ISF_drt` / `ISF_NL_drt` = RT difference in minutes.
-   `ISF_ratio` / `ISF_NL_ratio` = fragment intensity / precursor
    intensity.
-   `Adduct_annotation` = `adduct of ID <partner id(s)>` on both
    partners of an adduct pair; `Adduct_type` names the specific
    adduct species assigned to that feature.

-   **Exclude identified from ISF in export** — when ticked, the
    **Export Selected** download drops rows whose `ISF_annotation` OR
    `ISF_NL_annotation` is not `not ISF`.
-   **Reset selection** — clears the plot/table selection.

### 3.4 Processed feature table (full)

**Not** affected by plot selection — always the full processed list.

-   **Filter table results** — radio buttons for *only ISF*, *only
    non‑ISF*, or *both*. *only ISF* includes any feature with either
    a fragment or NL annotation.
-   **Editable `ISF_annotation`** — double‑click a cell to manually
    re‑tag. Edits are persisted and immediately reflected in the plots
    and export. *Should only be used if evidence exist on wrong tag.*
-   **Export Processed Table (Full CSV Columns)** — downloads the full
    table with all original columns plus every new ISF / NL / adduct
    column, respecting manual re‑tags.
    
### 3.7 Neutral‑loss and adduct annotation

**Neutral losses**: select any subset of the preset losses (H₂O
18.0106, NH₃ 17.0265, CO₂ 43.9898, SO₃ 79.9568, HCl 35.9767, HF
20.0062) and/or add custom entries via the `Label,mass;…` text field.
For every enabled loss X and every pair of features (j, i) in the
data:

-   If `|mz_i − mz_j − X| ≤ m/z tolerance (Da)` **and**
    `|rt_i − rt_j| ≤ RT tolerance`, then j is annotated as the
    neutral‑loss fragment of i (the heavier partner is treated as the
    precursor).
-   Multiple precursor candidates per fragment are stored
    comma‑separated, sorted by ascending `|ISF_NL_dDa|` (best match
    first).

**Adducts**: select any subset of preset adducts (`[M+H]+` 1.007276,
`[M+Na]+` 22.989218, `[M+K]+` 38.963158, `[M+NH₄]+` 18.033823,
`[M−H]−` −1.007276, `[M+HCOO]−` 44.998201, `[M+Cl]−` 34.969402)
and/or custom entries. For every unordered pair of adducts (a, b)
with masses (Mₐ, M_b), any two features (j, i) satisfying:

-   `|mz_i − mz_j − |M_b − Mₐ|| ≤ m/z tolerance (Da)` **and**
-   `|rt_i − rt_j| ≤ RT tolerance`

are flagged as adducts of the **same neutral molecule M**. The
lighter partner is labelled with the lighter adduct's name and the
heavier partner with the heavier one. Both partners are annotated in
`Adduct_annotation` (`adduct of ID <partner id>`) and `Adduct_type`.
**Adduct annotation overrides ISF**: if a feature is flagged as an
adduct, its `ISF_annotation`, `ISF_NL_annotation` and all associated
`dDa/drt/ratio` fields are cleared.

### 3.6 Sample comparison *(optional)*

Similar to the MD tab. Enable **Enable sample comparison** to be able to compare selected features (from other plots) across different samples.
Choose the number of samples and selected the samples intesnity columns.
Plot types: **Grouped bars** or **Lines + markers**.
-   Plot is drawn on **Plot sample comparison**; settings apply on
    click.
-   Any subsequent selection in the scatter or network plots
    auto‑refreshes the sample plot (capped at 40 features for
    legibility).
-   Legend format:
    `id=… | mz=… | rt=… | intensity=… | ISF_annotation=…`.


### 3.7 Tips

-   Start with a **wider m/z tolerance** (e.g. 0.02 Da) and RT
    tolerance to see all candidates, then tighten once the network
    looks reasonable.
-   Use the **network plot** to spot precursors with many linked
    fragments.
-   Use **lasso select** on the network plot to isolate a candidate
    precursor and its fragments in the feature table.
-   Curate false positives via the editable `ISF_annotation` column in
    the full feature table, then re‑export.
-   Tick **Exclude identified from ISF in export** to get a clean
    feature list for downstream statistics.



</div>
:::::

------------------------------------------------------------------------

# 4. Known Issues

-   Large data sets, >100-200 MB for the fragment file, can make ISF processing take a couple of minutes.
