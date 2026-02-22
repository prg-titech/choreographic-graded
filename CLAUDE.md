# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**choreographic-graded** is a research Haskell library implementing choreographic programming with graded types. It combines distributed systems programming with advanced type theory and effect systems to provide compile-time guarantees about distributed protocol correctness and resource usage.

## Development Commands

```bash
# Build the library and executable
cabal build

# Run the main executable (buyer-seller protocol demo)
cabal run choreographic-graded

# Run tests
cabal test

# Run examples
cabal run choreographic-graded     # Buyer-seller protocol demo
cabal run conclave-demo             # Conclave isolation demo
cabal run auction-demo             # Auction with local IO demo

# Build specific components
cabal build lib:choreographic-graded
cabal build exe:choreographic-graded
cabal build test:choreographic-graded-test
```

## Core Architecture

The codebase is organized into three main systems:

### 1. Choreography System (`src/Choreographic/Graded/`)

- **Choreography.hs** - Core choreography monad with graded effects for describing distributed protocols
- **Located.hs** - Values located at specific network nodes with type-level location tracking
- **Operation.hs** - Communication primitives (`comm`, `conclave`) for type-safe message passing
- **Runtime/Concurrent.hs** - Concurrent execution runtime for running choreographies

### 2. Graded Functor System (`src/Control/Functor/Graded/`, `src/Data/Functor/Graded/`)

- **GradedFunctor/GradedMonad** - Functors and monads with composable grade information
- **Effect/Coeffect** - Type classes for tracking computational effects and resource usage

### 3. Faceted Values System (`src/Choreographic/Graded/Faceted.hs`)

- **Faceted Values** - Location-specific values that can differ between locations (unlike Located values which are uniform)
- **IO Integration** - Support for location-specific IO operations via `foreach`

## Key Concepts

- **Choreography**: High-level description of distributed system behavior with automatic endpoint projection
- **Located Values**: `Located l a` represents uniform value `a` at locations `l`, tracked at the type level
- **Faceted Values**: `Faceted l a` represents location-specific values that can differ at each location in `l`
- **Communication**: `comm @location` for type-safe message passing between specific locations
- **Conclaves**: Restricted execution contexts for security/isolation
- **IO Operations**: `foreach` allows location-specific IO operations on Faceted values
- **Grades**: Type-level information about computational effects, resources, and constraints

## Language Extensions

This project uses advanced Haskell features extensively:

- **DataKinds, TypeFamilies, GADTs** - Type-level programming
- **RankNTypes, PolyKinds** - Higher-order type abstractions
- **TypeApplications** - Explicit type application for location specifications

## Development Patterns

1. **Free Monad Pattern** - DSL construction with `Free` from the `free` library
2. **Tagless Final Style** - Effect systems using type class constraints
3. **Phantom Types** - Location and grade information tracked only at type level
4. **GADT Pattern** - Existential types for communication operations and AST nodes

## Example Usage

The main executable demonstrates a buyer-seller protocol:

```haskell
program :: Choreography Univ Univ Bool
program = CFG.sub $ CFG.do
  name <- C.comm @"b" bName    -- Buyer sends name to seller
  price <- C.comm @"a" aPrice  -- Seller computes and sends price
  C.comm @"b" bIsBuy          -- Buyer decides whether to buy
```

This showcases location-aware computations, type-safe communication, and automatic protocol generation from high-level descriptions.
