# Welcome to the TeamTree Project

By Frank W. Pfrieger
CNRS / University of Strasbourg, Institute of Cellular and Integrative Neurosciences, Strasbourg, France.
(fw-pfrieger@gmx.de or frank.pfrieger@unistra.fr)

This repository provides free access to R code and example files enabling the TeamTree analysis. It also serves as launch pad for an open source project.

## What is this all about?
Here's the "scientific summary", for in-depth information see my BioRxiv preprint (Pfrieger, 2021, https://doi.org/10.1101/2020.06.01.128355):
"Advances in science and technology depend on the work of research teams and the dissemination of their results through peer-reviewed articles, which constitute a fastly growing socio-economic resource.
Current methods to mine publications in a field of interest focus on content, but the workforce credited by authorship remains largely unexplored.
This hurdle can be overcome by a new bibliometric approach that visualizes the publication records of authors working in a user-defined field, delineates their genealogic and collaborative connections and provides citation-independent performance measures.
This team-centered approach complements existing tools to mine the scientific literature across disciplines."

## Sounds too abstract?
Imagine that you are **interested in a specific topic of Science and Technology**, for example a rare human disease, life on Mars or quantum computing.
You will **query** specific **bibliometric databases** (PubMed, Scopus, Web of Science etc.) and **read scientific articles about the topic**.
However, this will **not give you the full picture**. You will have hard times to answer the following questions:
- How has field developed over time?
- How many people are working on the topic?
- Who are the key players, experts and potential collaborators - or competitors?

To overcome this hurdle, I have come up with the **TeamTree analysis**, further referred to as **TTA**.
This approach provides **new visuals and measures to explore a field of Science & Technology based on relevant publications.**
The current version of TTA is based on the open source programming langugage R.

This repository contains the R code to run TTA (Teamtree_8.txt) plus a list of PubMed publications (pub_clock.csv) and TTA-derived data for an exemplary field (Circadian Clock) as shown in the preprint (pub_clock_TTA_data.csv). The manuscript is currently under review.

## Installation
To run the TeamTree analysis on the exemplary field of research, you need to
1. Download the file with the R code (Teamtree_8.txt).
2. Download the file "pub_clock.csv" with the PubMed articles related to the keyword "Circadian clock" and the data file (pub_clock_TTA_data.csv).
3. Make sure you have the required libraries/packages.
4. In the R code, indicate the directories, where the csv file with the PubMed articles is located and where data should be saved to.
5. If you have downloaded the data file, TTA will use the saved colors. If not, you need to set "new colors=T".
6. Run the script by copy/paste in the RGui - or use your preferred method to run R code.

# Why an open source project?
The need to **find information about a field of S&T and to learn about the teams** working in the field **occurs frequently**. Thus, **TTA** is probably **of interest for a large and diverse range of users** such as - for example - scientists, engineers, programmers, but also editors, consultants, investors, journalists, medical doctors, philanthropists etc. Even the **general public**, I imagine someone trying to **find an expert on a rare disease**.
Therefore, the **open source project aims to make TTA available to anybody interested through a website or an app**. Since I am not a professional programmer, I decided to reach out for help through Github.

## ToDo list
- Revise R code to optimize for speed/memory.
- Alternatively: Implement the TeamTree procedure in a different language to improve performance/visual display and allow for interactivity.
- Create html page/app where a user can upload articles on a specific topic or query directly a relevant (and accessible) database (e.g. PubMed, Google Scholar, Microsoft Academic, search engines etc.).
- Extract authors and years of publication from the articles
- Perform TTA, store data and display the new visuals as shown in the preprint figures
- Develop new features and visuals such as
    - author disambiguation
    - 3D display of TeamTrees (spiral-like timeline)
    - interactive graphs allowing a user for example to obtain information on specific teams, to zoom into a specific time window.
- Application of TTA to other types of scientific output such as patents and preprints.

