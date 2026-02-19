# Hasxiom: Pure Functional Lakehouse Engineering

## Overview
**Hasxiom** is a high-performance, strictly typed data ingestion and analysis engine built for the Nix ecosystem. Unlike traditional imperative ETL pipelines, Hasxiom treats infrastructure as code and data transformation as a pure function: $(data) \to \text{Insights}$.

The project currently automates the ingestion, enrichment, and tree-analysis of over 13,000+ nodes within the Nix ecosystem, mapping the complex dependencies of the world's most sophisticated package manager.

## Core Inspirations & Intent
Hasxiom is a rigorous exercise in functional mastery, synthesized from three primary drivers:
* **Architectural Philosophy:** Heavily inspired by the work of **Alex Vieth**, specifically the application of axiomatic DSLs and high-assurance functional patterns. We aim for "Correctness by Construction."
* **The 13k Node Challenge:** Inspired by **Malte's** technical breakdown of the Nix package graph. Malte identifies that while there are roughly 17,000 packages on Hackage, only about 7,000 typically build in the main Nix packages set. This project addresses the scaling and dependency issues inherent in managing 13,000+ nodes using pure recursion to navigate the web without the memory leaks or stack overflows often associated with global coherency challenges.
* **Professional Milestone:** This codebase serves as the primary technical portfolio for application to **Mercury Financial**. It demonstrates the ability to manage complex distributed systems and large-scale data sets using the most stringent FP constraints.

## Distributed Infrastructure (The NixOS Grid)
Our engineering environment consists of two physical NixOS nodes linked via a dedicated high-speed LAN.

### Node A: The Engineering Station (Ultra 7)
* **Role:** Development, GHC Compiling, Git Orchestration.
* **IP:** `192.168.68.53`
* **Specs:** Intel Ultra 7, 64GB DDR5, NVIDIA RTX 3060.

### Node B: The Data Engine (nixlakehouse)
* **Role:** PostgreSQL Ingestion, Massive Node Processing, Persistent Storage.
* **IP:** `192.168.68.56` (Postgres Host)
* **Specs:** Dell Latitude E5570, 24GB DDR4.

## The Four Tenets of Hasxiom Engineering
As a pure functional project, Hasxiom adheres to these non-negotiable standards:
1. **Total Functions Only:** No `null` values; every input has a defined, returned output.
2. **Immutability:** Data is never "changed" in place; new states are evolved through recursion.
3. **Pure Logic:** Side effects are strictly isolated within the IO Monad at the edges.
4. **Declarative Infrastructure:** Everything is driven by `flake.nix` to ensure mathematical reproducibility.

## Technical Stack
* **Language:** Haskell (GHC 9.6+)
* **Database:** PostgreSQL (Hosted on `nixlakehouse`)
* **Environment:** NixOS with Flakes
* **Complexity Goal:** $O(n)$ for ingestion and $O(\log n)$ for tree traversal.

## Current Progress: The Malte Milestone
The system has successfully ingested the Nix dependency graph into a relational lakehouse, solving the scale issues inherent in 13,000+ node traversals identified in Malte's research.
* **Enrichment:** Automated metadata tagging for all nodes.
* **Impact Analysis:** Determining the "blast radius" of package updates within the graph using tail-recursive exploration.
