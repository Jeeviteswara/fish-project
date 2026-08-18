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

Catch Calculator — Compares a user-entered catch with historical catch values and calculates the percentage change from the last known value.

Interactive Charts — Visualizes catch trends and comparisons using Chart.js.

User Login — Provides a simple login flow with name, email, and password validation.

Responsive Interface — Designed to work across different screen sizes.

Technologies Used

Haskell

Scotty — Web framework

Cassava — CSV parsing

Aeson

ByteString

Vector

Warp

Chart.js

HTML / CSS / JavaScript

Docker

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

The application uses a CSV dataset containing marine fish catch records organized by:

Species | Area | Year | Catch

The dataset is stored in:

data/data1.csv

Sustainability Analysis

The application processes fish catch records by:

Filtering records by species and area.

Ordering records chronologically.

Calculating year-to-year catch changes.

Counting declining years.

Classifying the overall sustainability trend.

Trend Classification

Classification

Description

Stable or Improving

Fewer than half of the year-to-year changes are negative

Mixed Trend

Half of the year-to-year changes are negative

Declining Trend

More than half of the year-to-year changes are negative

Not Enough Data

No year-to-year comparison is available

Running Locally

Prerequisites

GHC / Haskell

Cabal

Git

Clone the Repository

git clone https://github.com/Jeeviteswara/fish-project.git
cd fish-project

Build and Run

cabal update
cabal build
cabal run

The application runs on port 3000 by default.

Open:

http://localhost:3000

Docker

The project includes a Dockerfile for containerized deployment.

Build the image:

docker build -t fish-project .

Run the container:

docker run -p 3000:3000 fish-project

Then open:

http://localhost:3000

Live Demo

🌐 Fish Stock Sustainability — Live Demo

The free hosting instance may take some time to respond after a period of inactivity.

Repository

💻 GitHub Repository

Project Purpose

This project demonstrates the use of functional programming, recursive data processing, CSV parsing, web development, and data analysis to build an interactive application for studying fish-stock sustainability.
