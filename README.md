Fish Stock Sustainability

A Haskell-based web application for analyzing marine fish catch data and identifying sustainability trends across species and fishing areas.

Overview

Fish Stock Sustainability provides an interactive dashboard for exploring historical fish catch records. Users can select a fish species and area, analyze year-by-year catch changes, compare species or areas, rank species-area combinations by declining years, and calculate changes against historical catch values.

The application uses a Haskell backend with recursive data-processing functions and a CSV dataset.

Features

Interactive Dashboard — Search for a fish species and select its corresponding area for analysis.

Sustainability Analysis — Calculates year-to-year catch changes and classifies trends as:

Stable or improving

Mixed trend — needs monitoring

Declining trend — possible sustainability risk

Dataset Overview — Displays the CSV records directly in the application.

Species Comparison — Compare the historical catch trends of two species in the same area.

Area Comparison — Compare the same species across two different areas.

Risk Ranking — Ranks species-area combinations according to the number of declining years.

Catch Calculator — Compares a user-entered current catch with historical catch values and calculates percentage change from the last known value.

Interactive Charts — Visualizes catch trends and comparisons using Chart.js.

User Login — Provides a simple login flow with name, email, and password validation.

Responsive Interface — Dashboard pages adapt to smaller screens.

Methodology

The application processes each CSV record into a structured FishRecord containing:

Species

Area

Year

Catch

The analysis pipeline is:

Parse the CSV dataset.

Filter records by species and area.

Sort records chronologically.

Calculate year-to-year catch differences recursively.

Count the number of declining years.

Classify the trend based on the proportion of negative yearly changes.

Present the results through tables, statistics, rankings, and graphs.

Sustainability Classification

The application uses the following decision logic:

Declining trend — possible sustainability risk: More than half of the year-to-year changes are negative.

Mixed trend — needs monitoring: Half of the year-to-year changes are negative.

Stable or improving trend: Fewer than half of the year-to-year changes are negative.

Not enough data: No year-to-year comparison is available.

Technologies Used

Haskell

Scotty — Web framework

Cassava — CSV parsing

Aeson

ByteString

Vector

Warp

Chart.js — Interactive graphs

HTML/CSS/JavaScript

Docker — Containerized deployment

Project Structure

fish-project/
├── app/
│   └── Main.hs
├── data/
│   └── data1.csv
├── Dockerfile
├── fish-project.cabal
├── nixpacks.toml
└── README.md

Dataset

The application uses a CSV dataset containing marine catch records organized by:

Species | Area | Year | Catch

The application reads the dataset from:

data/data1.csv

Running Locally

Prerequisites

GHC / Haskell

Cabal

Git

Clone the repository

git clone https://github.com/Jeeviteswara/fish-project.git
cd fish-project

Build and run

cabal update
cabal build
cabal run

The application runs on port 3000 by default.

Open:

http://localhost:3000

The application also reads the PORT environment variable when deployed.

Docker

The project includes a multi-stage Dockerfile.

Build the image:

docker build -t fish-project .

Run the container:

docker run -p 3000:3000 fish-project

Then open:

http://localhost:3000

Live Demo

https://fish-project-gtii.onrender.com

The free hosting instance may take some time to respond after a period of inactivity.

Repository

GitHub: https://github.com/Jeeviteswara/fish-project

Project Purpose

The project demonstrates how functional programming concepts, recursive algorithms, structured data processing, and web development can be combined to create an interactive data-analysis application for studying fish-stock sustainability.
