# Iteration Analysis

Analysis of the reformat pipeline's iteration/convergence
behavior, run against the project's own codebase (src/ and
test/ — 23 files).

## Pipeline

**Note:** This analysis was performed on the original 8-step pipeline.
The pipeline has since been simplified to:

1. Collapse
2. Forced breaks (position-checked)
3. Pair breaking (all types, unified)
4. fix-source
5. Forced breaks (unchecked)
6. Conditional re-run of steps 3-4

The non-binding/binding pair-breaking split was unified (no
behavioral change). Multiline-child breaking was removed
(fix-source already handles these cases). The original
analysis data is preserved below for reference.

Original analysis (8-step pipeline, 23 files):

- Total pipeline iterations: 47 across 23 files
- Max iteration count: 2 (check.clj; all others converge
  in 1)

Changed steps frequency (across all pipeline iterations):

| Step | Count |
|------|-------|
| forced-breaks-checked | 24 |
| fix-source-1 | 21 |
| binding-pair-breaking | 17 |
| forced-breaks-unchecked | 12 |
| non-binding-pair-breaking | 6 |
| multiline-child-1 | 2 |
| multiline-child-2 | 1 |

## Forced breaks sub-loop

The most iteration-heavy loop. Processes one form per
iteration (find first, edit, re-parse, repeat).

- Total events: 1102 (1011 form-processing + 91 stable)
- Max iterations to stable: **90** (reformat.clj)
- All forms are `:list_lit`
- High iteration counts per file: reformat.clj (90),
  main.clj (41), node.clj (30), check.clj (26),
  language.clj (23), config.clj (18), fix.clj (16),
  cli.clj (13)

This is the biggest convergence bottleneck — it re-parses
the entire file after each single form edit. Files with
many `defn` forms need many iterations.

## Pair breaking sub-loop

- Total events: 238 (144 form-processing + 94 stable)
- Max iterations to stable: 27
- Form types: `:map_lit` (most common), `:vec_lit`,
  `:list_lit`

## Fix-source sub-loop

- Total events: 204 (94 stable + 110 iteration events)
- Max iterations to stable: 14
- The trace events at this level only have `:long-line-count`
  (no individual form info — forms are traced via the
  stale-indent path if relevant)

## Multiline child sub-loop

- Total events: 102 (68 form-processing + 34 stable)
- Max iterations to stable: 2
- Form types: `:list_lit`, `:map_lit`, `:vec_lit`
- Low iteration count — this loop converges quickly

## Stale indent detection

**Never triggers.** 0 events across all src/ and test/
files. This code path exists as a fallback in
`break-on-line` (fix.clj:929) but does not activate on
the project's own codebase. It may be needed for edge
cases in user code, or it may be dead code.

## Unchecked forced breaks (2nd pass)

The second `apply-forced-breaks` call (with
`check-position? false`) triggers in 12 out of 47
pipeline iterations (26%). It processes 39 forms across
those calls. This pass is **not dead code** — it catches
forms that were initially mid-line (not at line start)
and only became breakable after earlier pipeline steps
moved them.

## Key findings

1. **Forced breaks is the bottleneck**: up to 90 iterations
   for a single file, processing one form at a time with a
   full re-parse each time. Batch processing would help.
   (Resolved: inner loops now batch edits.)

2. **Pipeline converges fast**: max 2 outer iterations. The
   inner loops do the heavy lifting. (Resolved: outer loop
   replaced with conditional re-run.)

3. **Stale indent never triggers**: candidate for removal
   or at minimum investigation of whether any real-world
   code triggers it.

4. **Unchecked forced breaks are load-bearing**: 26% of
   pipeline iterations benefit from the second pass. Cannot
   be removed without alternative.

5. **fix-source-2 and multiline-child-2 are nearly dead**:
   (Resolved: multiline-child removed entirely, fix-source
   handles these cases. Second fix/multiline passes
   eliminated.)

6. **Single-form-per-iteration pattern**: forced-breaks,
   pair-breaking, and multiline-child all find one form,
   edit it, re-parse the whole file, and repeat. Batching
   multiple non-overlapping edits per parse would reduce
   iteration counts significantly. (Resolved: all inner
   loops now batch non-overlapping edits.)
