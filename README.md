# Hasxiom: Sovereign AI & Pure Functional Lakehouse Engineering

## Overview
**Hasxiom** is a high-performance, strictly typed data ingestion and analysis engine built for the Nix ecosystem. Unlike traditional imperative ETL pipelines, Hasxiom treats infrastructure as code and data transformation as a pure function: $(data) \to \text{Insights}$.

The project has successfully automated the ingestion of over **17,000+ nodes**, mapping the 119-layer deep dependency tree of the Nix ecosystem into a persistent SQL Lakehouse.

## The Mission: Solving the 13k Node Challenge
Inspired by **Malte Ott’s** technical breakdown of Nixpkgs and the axiomatic patterns of **Alex Vieth**, Hasxiom bridges the "Coherency Gap." We have pivoted our architecture to handle the massive computational load of graph traversal by offloading relational logic to a dedicated Lakehouse and GPU-accelerated analysis to the Ultra 7. I was inspired to create Hasxiom DSL to relieve these pain points.

### Core Strategic Pillars:
1. **Hasxiom DSL:** A custom language utilizing recursive combinators to define high-assurance logic for dependency resolution.
2. **Local AI & ML Stack:** Utilizing **Ollama (CUDA-accelerated)** and **Libtorch** to perform local LLM analysis on dependency graphs, keeping all data sovereign and local.
3. **The RTX Galaxy:** Leveraging the **NVIDIA RTX 3060** for real-time visualization of complex 119-layer dependency clusters.
4. **Mercury Financial Standard:** Adherence to the Four Tenets of pure FP, meeting the rigorous requirements of top-tier Haskell firms.

## Distributed Infrastructure (The Sovereign Grid)
Our environment is a distributed NixOS cluster, balancing specialized nodes for data and compute.

### Node A: The Powerhouse (Ultra 7 / Phoenix)
* **Role:** GHC Compilation, Local AI Orchestration (Ollama/Cuda), and GPU-Accelerated Analysis.
* **IP:** `192.168.68.53`
* **Hardware:** * **CPU:** Intel Ultra 7 (16-core architecture)
    * **RAM:** 64GB DDR5
    * **GPU:** NVIDIA RTX 3060 (12GB VRAM) - **Active CUDA 12.x Stack**
* **Local AI:** Running **Ollama-CUDA** for sovereign LLM reasoning over the codebase.

### Node B: The Data Engine (nixlakehouse)
* **Role:** PostgreSQL Storage, Massive Store Crawling, Persistent Lakehouse.
* **IP:** `192.168.68.56`
* **Hardware:** Dell Latitude E5570 (Intel i7, 24GB RAM).
* **DB:** PostgreSQL 16 (Optimized for Recursive CTEs).

## The Four Tenets of Hasxiom
1. **Total Functions Only:** No `null` values; `postgresql-simple` ensures type-safe DB interactions.
2. **Immutability:** States are evolved through tail-recursion; data is never mutated.
3. **Pure Logic:** Side effects are strictly isolated; infrastructure is declared via **Nix Flakes**.
4. **Sovereignty:** All data, models (Ollama), and pins are local. No cloud dependency.

## Current Project State (February 2026)
* **Status:** Solidified Infrastructure.
* **Layers:** 119 Depth Levels Mapped.
* **Pillars:** 17 Core Surgical Pins Identified (including `aeson`, `ghc`, `postgresql-simple`).
* **Visuals:** RTX-Rendered 2000-edge "Galaxy" Map generated.

---
*Maintained by Scott Baker (NixOSDude)*
