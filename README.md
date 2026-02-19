# Hasxiom: Pure Functional Lakehouse Engineering

## Overview
**Hasxiom** is a high-performance, strictly typed data ingestion and analysis engine built for the Nix ecosystem. Unlike traditional imperative ETL pipelines, Hasxiom treats infrastructure as code and data transformation as a pure function ((data) \rightarrow \text{Insights}$).

The project currently automates the ingestion, enrichment, and tree-analysis of over 13,000+ nodes within the Nix ecosystem, mapping the complex dependencies of the world's most sophisticated package manager.

## Distributed Infrastructure (The NixOS Grid)
Our engineering environment consists of two physical NixOS nodes linked via a dedicated high-speed LAN, balancing legacy stability with modern computational power.

### Network Topology
* **Interconnect:** 5-Port Gigabit Ethernet Switch (Unmanaged)
* **Subnet:** `192.168.68.0/24`

### Node A: The Engineering Station (Ultra 7)
* **Role:** Development, GHC Compiling, Git Orchestration, GPU-Accelerated Analysis.
* **IP:** `192.168.68.53`
* **Specs:**
    * **CPU:** Intel Ultra 7 (Cutting-edge architecture)
    * **RAM:** 64GB DDR5
    * **GPU:** NVIDIA RTX 3060 (12GB VRAM) - Active/Registered in NixOS
    * **Storage:** 1TB 5th Gen PCIe NVMe SSD

### Node B: The Data Engine (nixlakehouse)
* **Role:** PostgreSQL Ingestion, Massive Node Processing, Persistent Storage.
* **Model:** Dell Latitude E5570 (9-year-old legacy workhorse)
* **IP:** `192.168.68.56`
* **Specs:**
    * **CPU:** Intel Core i7
    * **RAM:** 24GB DDR4
    * **Storage:** 1TB SATA SSD

## The Four Tenets of Hasxiom Engineering
As a pure functional project, Hasxiom adheres to these non-negotiable standards:
1. **Total Functions Only:** No `null` values; every input has a defined, returned output.
2. **Immutability:** Data is never "changed" in place; new states are evolved through recursion.
3. **Pure Logic:** Side effects are strictly isolated within the IO Monad at the edges of the system.
4. **Declarative Infrastructure:** Everything is driven by `flake.nix` to ensure "it works on my machine" is a mathematical certainty.

## Technical Stack
* **Language:** Haskell (GHC 9.6+)
* **Database:** PostgreSQL (Hosted on nixlakehouse)
* **Environment:** NixOS with Flakes
* **Complexity Goal:** $O(n)$ for ingestion and $O(\log n)$ for tree traversal.

## Current Progress: The 13,000 Node Milestone
The system has successfully ingested the Nix dependency graph into a relational lakehouse. 
* **Enrichment:** Automated metadata tagging for all nodes.
* **Tree Analysis:** Recursive traversal of package dependencies.
* **Impact Analysis:** Determining the "blast radius" of package updates within the graph.

## How to Run
Ensure you are within the Nix environment to pull all Haskell dependencies:

```bash
nix-shell
cabal run hasxiom-ingest
```

---
*Maintained by Scott Baker (NixOSDude)*
