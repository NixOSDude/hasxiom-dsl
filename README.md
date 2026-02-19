# Hasxiom: Pure Functional Lakehouse Engineering

## Overview
**Hasxiom** is a high-performance, strictly typed data ingestion and analysis engine built for the Nix ecosystem. Unlike traditional imperative ETL pipelines, Hasxiom treats infrastructure as code and data transformation as a pure function: $(data) \to \text{Insights}$.

The project currently automates the ingestion, enrichment, and tree-analysis of over 13,000+ nodes within the Nix ecosystem, mapping the complex dependencies of the world's most sophisticated package manager.

## The Mission: Solving the 13k Node Challenge
The primary purpose of Hasxiom is to bridge the "Coherency Gap" in the Haskell-Nix ecosystem, a challenge famously detailed in **Malte’s** technical breakdown of Nixpkgs. I was inspired to create/develop Hasxiom DSL to relieve this pain point.

While Hackage contains ~17,000 packages, the global coherency constraints of Nixpkgs often limit the buildable set to ~7,000. Hasxiom addresses the scaling and dependency issues inherent in managing these 13,000+ nodes by synthesizing three core pillars:

1. **Hasxiom DSL:** A custom-created language for axiomatic functional logic, implementing **Alex Vieth’s** patterns to define high-assurance logic for dependency resolution.
2. **HasTorch Integration:** Utilizing `hastorch` for tensor-based graph analysis, leveraging the **Ultra 7's NVIDIA RTX 3060** to perform high-speed computations on the dependency matrix.
3. **Mercury Financial Standard:** Maintaining a codebase that adheres to the "Four Tenets" of pure FP, meeting the rigorous engineering requirements of top-tier Haskell firms.



## Distributed Infrastructure (The NixOS Grid)
Our engineering environment consists of two physical NixOS nodes linked via a dedicated high-speed LAN, balancing legacy stability with modern computational power.

### Node A: The Engineering Station (Ultra 7)
* **Role:** Development, GHC Compiling, Git Orchestration, and GPU-Accelerated Analysis.
* **IP:** `192.168.68.53`
* **Specs:**
    * **CPU:** Intel Ultra 7 (Cutting-edge architecture)
    * **RAM:** 64GB DDR5
    * **GPU:** NVIDIA RTX 3060 (12GB VRAM) - Active/Registered in NixOS
    * **Storage:** 1TB 5th Gen PCIe NVMe SSD
    * **OS:** NixOS

### Node B: The Data Engine (nixlakehouse)
* **Role:** PostgreSQL Ingestion, Massive Node Processing, Persistent Storage.
* **IP:** `192.168.68.56`
* **Specs:**
    * **Model:** Dell Latitude E5570 (Legacy workhorse)
    * **CPU:** Intel Core i7
    * **RAM:** 24GB DDR4
    * **Storage:** 1TB SATA SSD
    * **OS:** NixOS

## The Four Tenets of Hasxiom
1. **Total Functions Only:** No `null` values; every input has a defined, returned output.
2. **Immutability:** States are evolved through pure recursion; data is never mutated in place.
3. **Pure Logic:** Side effects are strictly isolated within the IO Monad at the system boundaries.
4. **Declarative Infrastructure:** Everything is driven by Nix Flakes to ensure mathematical reproducibility.

## Technical Stack
* **Languages:** Haskell (GHC 9.6+), Hasxiom DSL
* **Tensor Library:** HasTorch (Libtorch bindings)
* **Database:** PostgreSQL (Hosted on `nixlakehouse`)
* **Environment:** NixOS (Pinned via Flakes)
* **Complexity:** $O(n)$ Ingestion / $O(\log n)$ Traversal.

---
*Maintained by Scott Baker (NixOSDude)*
