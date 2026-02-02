# BLG 458E - Functional Programming (Haskell) Coursework

This repository contains the homework assignments and the term project for the **BLG 458E Functional Programming** course at Istanbul Technical University. The coursework explores complex mathematical and social simulations using the **Haskell** programming language.

## Programming Language & Environment
* [cite_start]**Primary Language**: [Haskell](https://www.haskell.org/) [cite: 779, 866, 964]
* **Supporting Tools**: 
    * [cite_start]**Julia**: Used for generating grid maps and initial simulation parameters[cite: 771, 967].
    * [cite_start]**CloudCompare**: Used for 3D visualization of STL files generated via Haskell[cite: 836].
    * **GHC**: The Glasgow Haskell Compiler is used to build the simulation executables.

---

## Coursework Overview

### [Homework 1: Hilbert Curves](https://github.com/FeyFey01/Functional-Programming-HW-Solutions/tree/main/hw1)
* [cite_start]**Task**: Implement a recursive algorithm to generate **Hilbert Curves**, a type of space-filling fractal[cite: 818, 820].
* [cite_start]**Solution Value**: Demonstrates mastery of **recursive decomposition** and 3D modeling by outputting line segments into **STL files**[cite: 832, 833, 835].

### [Homework 2: Schelling Segregation Model](https://github.com/FeyFey01/Functional-Programming-HW-Solutions/tree/main/hw2)
* [cite_start]**Task**: Simulate social segregation based on the agent-based model introduced by Thomas Schelling[cite: 760, 762].
* **Solution Value**: Uses functional paradigms to process 2D grid maps. [cite_start]Agents (represented as tortoises) move to new locations based on a **tolerance threshold** for similar neighbors[cite: 764, 766, 767].

### [Homework 3: Shortest Distance in Knowledge Graphs](https://github.com/FeyFey01/Functional-Programming-HW-Solutions/tree/main/hw3)
* [cite_start]**Task**: Find the shortest path and distance between two entities (MIDs) in a collaborative knowledge base (Freebase)[cite: 941, 942, 964].
* **Solution Value**: Focuses on **graph theory** and data processing. [cite_start]The solution must handle a large dataset of triplets and return the full path between nodes or throw an error if no connection exists[cite: 946, 965].

### [Term Project: Cellular Automata - Game of Life](https://github.com/FeyFey01/Functional-Programming-HW-Solutions/tree/main/term-project)
* [cite_start]**Task**: Implement **Conway's Game of Life** to simulate biological self-replication and complex oscillators[cite: 852, 854].
* **Solution Value**: Explores non-linear dynamics and state evolution. [cite_start]The project requires generating five specific **oscillator patterns** that return to their original state after a fixed period[cite: 863, 866].
