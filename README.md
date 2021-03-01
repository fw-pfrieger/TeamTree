# Welcome to the TeamTree Project

By Frank W. Pfrieger

CNRS/University of Strasbourg, Strasbourg, France
(fw-pfrieger@gmx.de or frank.pfrieger@unistra.fr)

This repository provides free access to R code and example files (see preprint Pfrieger, 2021 https://doi.org/10.1101/2020.06.01.128355) and serves as launch pad for an open source project.

## What's this all about?
Here's the "scientific summary" from my preprint:
"Advances in science and technology depend on the work of research teams and the dissemination of their results through peer-reviewed articles, which constitute a fastly growing socio-economic resource.
Current methods to mine publications in a field of interest focus on content, but the workforce credited by authorship remains largely unexplored.
This hurdle can be overcome by a new bibliometric approach that visualizes the publication records of authors working in a user-defined field, delineates their genealogic and collaborative connections and provides citation-independent performance measures.
This team-centered approach complements existing tools to mine the scientific literature across disciplines."

## Sounds too abstract?
Ok, imagine you are a consultant, editor, engineer, investor, journalist, medical doctor, philanthropist, a scientist like me, or you-name-it.
Imagine further that you are **interested in a specific topic of Science and Technology**, for example a rare human disease, research on Mars or the latest about quantum computers.
You will query specific bibliometric databases (PubMed, Google Scholar etc.) and **read scientific articles about the topic**.
However, this will **not give you the full picture**. You will have hard times to answer the following questions:
- How has field developed over time?
- How many people are working on the topic?
- Who are the key players, experts and potential collaborators - or competitors?

To overcome this hurdle, I have come up with the **TeamTree analysis**, further referred to as **TTA**.
This approach provides **new visuals and measures to explore a field of Science & Technology based on relevant publications.**
For various reasons, I have implemented TTA using the open source programming langugage R.

This repository contains the R code that implements the current version of TTA plus PubMed publications and TTA-derived data for an exemplary field to illustrate the approach.

## Installation
To run the TeamTree analysis on the exemplary field of research, you need to
1. Download the file with the R code (TeamTree.txt).
2. Download the csv file with the PubMed articles related to "Circadian clock".
3. Make sure you have the required libraries/packages
4. In the R code, indicate, where the csv file with the PubMed articles is located.
5. Run the script by copy/paste in the RGui - or use your preferred method to run R code.

# Why an open source project?
I am not a professional programmer, so I decided to reach out for help. The goal of the project is to set up a freely accessible website that allows anybody interested to perform the TeamTree analysis.

Here's my ToDo list, merely a suggestion:
- Implementation of the TeamTree procedure in a different language to improve performance/visual display
- Create html page where a user can upload articles on a specific topic or query directly a relevant database (e.g. PubMed, Google Scholar, Microsoft Academic, preprint servers, search engines etc.).
- Extract authors and years of publication from the articles
- Perform TTA, store data and display the different graphs as shown in the preprint
- Develop new features and visuals such as author disambiguation, three-dimensional display of TeamTrees and interactive graphs allowing a user to obtain information on a specific team.
