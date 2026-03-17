# Stoney Analysis

Repository for organizing documents, data extracts, notes, and analysis files related to the Stoney NIJ fingermark project.

## Structure

- `docs/`: Quarto and other analysis documents
- `data/`: project data files or cleaned extracts
- `notes/`: meeting notes, task summaries, and working memos

## Current Contents

- `analysis/statistical-analysis.qmd`: Quarto source for the two-part analysis of NIFM occurrence and mark informativeness
- `docs/index.html`: rendered page for GitHub Pages publishing

## Publish Online

To make the analysis visible on the internet with GitHub Pages:

1. Commit and push the repository to GitHub.
2. In the GitHub repository, open `Settings` > `Pages`.
3. Under `Build and deployment`, choose `Deploy from a branch`.
4. Select the `main` branch and the `/docs` folder.
5. Save the settings and wait for GitHub to publish the site.

After publication, the page will be served from the repository's GitHub Pages URL, using `docs/index.html` as the homepage.

## Next Steps

1. Move source documents and notes from the older project folder into this repository.
2. Add any data dictionaries or provenance notes for the grouped data files.
3. Decide whether to store raw data directly here or keep only cleaned analysis-ready extracts.
