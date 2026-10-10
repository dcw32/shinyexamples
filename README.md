# Calder Island Cat Lab

Interactive HTML training tool that teaches the broking team how catastrophe models work and why it matters to their book. Built 10 Oct 2026. Published as a private artifact titled "Calder Island Cat Lab" (version 2 = Phase 1 + Phase 2).

## Motivation

- Audience is a "sales mindset" broking team, so the aim is intuition, not maths: where losses come from and who pays for them.
- The model chain is taught in the order a cat model runs: exposure, hazard, vulnerability, financial terms. Each stage changes something visible on the map.
- Phase 2 turns the mechanics into three conversations a broker actually has: accumulation, diversification, adverse selection.
- Everything is fictional (island, curves, event set, currency-neutral "m" units) so nobody mistakes it for a vendor view of risk and there are no licensing questions.

## Layout

- Top bar: Phase 1 and Phase 2 step chips, Back/Next.
- Guide strip: step title, short explanation, "Try" prompts, a live "Notice" line (numbers update as you change things), optional action chips.
- Map (centre-left): layer toggles above (relief, slide-prone slopes, wind footprint, landslide footprint, damage, value concentration), event chips and sliders below (intensity, storm position, Play). Competitor card appears under the map in step 7 only.
- Right panel: three headline figures (this event, average annual loss, 1-in-100) with change chips, a "who pays" bar, a Phase 2 pricing strip (steps 5-7 only), then tabs: Exposure, Vulnerability, Results.

## Sections (steps)

Phase 1, mechanics
1. Exposure: place buildings, load preset books, see TIV and value concentration. Four large sites are pre-placed: Calder Port and Marram Refinery (west coast, 2 km apart), Hartley Hydro Plant (slide-prone valley), Eastmere Distribution Park (east coast).
2. Hazard: four events (Storm Aurora, Storm Brage, Atlantic rain, Southern gale) with footprints; scrub or Play the storm along its track.
3. Vulnerability: damage curves per construction type for wind and landslide, test-intensity slider, marker for the selected building.
4. Financial: deductible (% of TIV), policy limit (or none), participation (co-insurance share), applied to one risk, one type or the whole book. Defaults: 1% deductible, no limit, 100% participation.

Phase 2, applications
5. Accumulation: cap limits on the port, refinery and hydro plant; watch the 1-in-100 fall against the premium given up.
6. Diversification: add 35 east coast risks; compare TIV, average annual loss and 1-in-100 with the pinned start.
7. Adverse selection: flat-rate book, competitor with a landslide map takes the lowest slide-risk small risks first; loss ratio rises. Switch to risk-based pricing or re-price the flat rate.

## How the model works (engine)

- Island: polar outline, storms enter from the west. Terrain is a sum of Gaussian hills; landslide susceptibility (0 to 1) is a set of valley Gaussians modulated by a patchy factor.
- Event set: 3,000 synthetic events, fixed seed (20261010), total rate 1.5 a year. Westerly storms (exponential wind tail, cap 72 m/s), easterly storms (about 35% of storms), and rain-only events (about 28%).
- Wind swath: peak gust falls off with distance from the track and decays inland. Rain follows a wider swath with an elevation boost. Landslide index = susceptibility x rainfall.
- Vulnerability: wind uses Weibull curves [scale, shape, max damage] per type; landslide uses a smooth-step on the index (threshold 0.3). Combined damage = 1 - (1 - wind)(1 - landslide).
- Financial terms: ground-up loss, minus deductible, capped at limit, times participation.
- Metrics: average annual loss and exceedance curve come from sorting the 3,000 event losses; 1-in-N uses linear interpolation on cumulative rate.
- Premium: risk-based = each risk's own expected loss x 1.4 (LOAD 0.4). Flat = one rate on TIV, set from the book when the competitor share is 0.
- Calibration targets (west coast book): loss cost about 0.55% of TIV, 1-in-100 about 10% of TIV, wind about 47% of average annual loss. Tail is led by Hartley Hydro Plant and Calder Port.

## Where to change things

The source is three parts concatenated into one HTML file:

| Part | Contains | Change here for |
|---|---|---|
| shell.html | CSS tokens (light and dark), page markup | colours, fonts, layout, wording in static markup |
| engine.js | geometry, hazard, vulnerability, financial terms, event set, portfolio run, premium, competitor | model numbers, curves, event frequency, site positions and TIV |
| ui.js | state, map drawing, panels, STEPS array, actions | guide text, step set-up, chips, new features |

Common edits
- Guide wording: the `STEPS` array in ui.js (`title`, `lede`, `tries`, `chips`, `notice()`).
- Event scenarios: `SCN` in engine.js. Large sites: `siteDefs()` (name, TIV, position).
- Vulnerability curves: `TYPES` in engine.js. Building values: `tiv` in `TYPES`.
- Event frequency and severity: `makeEventSet()` and `TOTAL_RATE`. After any change, rerun the calibration check and confirm the targets above still hold, because landslide and wind balance is sensitive.
- Premium loading: `LOAD` in engine.js.
- Adding a step: push a new object onto `STEPS` with `setup()` and `notice()`; set `phase: 2` for the application group. Step 7 (index 6) is special-cased for the flat-rate premium and competitor card, so keep it as index 6 or update those checks.

## Build and publish

1. Edit the part files.
2. Concatenate: shell.html, then `<script>` + engine.js + `</script>`, then `<script>` + ui.js + `</script>`, into calder.html.
3. Syntax-check the combined script with `node --check`.
4. Optional: run the engine in node to check calibration numbers, and load the page headlessly to catch runtime errors.
5. Publish the same calder.html path to the existing artifact so the link stays the same.

If the working files are gone, the published artifact is the source of truth: read it back, split the inline scripts out, and carry on.

## Known limits and ideas

- Fictional hazard and curves; not calibrated to any vendor model.
- Sharing: the artifact is private until shared from its Share menu.
- Fonts load from Google Fonts; offline the page falls back to system fonts.
- Ideas: quota share or excess-of-loss reinsurance layer, a "role" lens for client servicing (claims and premium flow), a third peril, an exportable summary for session notes, saved scenarios.
