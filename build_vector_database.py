"""
Step 1: Build Vector Database for Executive Orders RAG System

This script takes the cleaned EO chunks and creates semantic embeddings,
storing them in ChromaDB for fast similarity search.

KEY CHANGE: Using COSINE similarity instead of Euclidean distance
for more intuitive similarity scores (0-1 scale).
"""

import pandas as pd
from sentence_transformers import SentenceTransformer
import chromadb
from chromadb.config import Settings
import os

print("=" * 60)
print("BUILDING VECTOR DATABASE FOR EXECUTIVE ORDERS")
print("=" * 60)

# -----------------------------------------------------------------------------
# STEP 1: Load the data
# -----------------------------------------------------------------------------
print("\n[1/4] Loading CSV data...")

df = pd.read_csv('eo_chunks_final.csv')

print(f"✓ Loaded {len(df)} chunks from {df['eo_number'].nunique()} Executive Orders")
print(f"  Date range: {df['signing_date'].min()} to {df['signing_date'].max()}")

# -----------------------------------------------------------------------------
# STEP 2: Initialize the embedding model
# -----------------------------------------------------------------------------
print("\n[2/4] Loading embedding model...")
print("  Model: all-MiniLM-L6-v2 (384 dimensions)")
print("  This converts text into semantic vectors...")

# This model was trained on billions of text pairs to learn semantic similarity
model = SentenceTransformer('all-MiniLM-L6-v2')

print("✓ Embedding model loaded")

# -----------------------------------------------------------------------------
# STEP 3: Generate embeddings for all chunks
# -----------------------------------------------------------------------------
print("\n[3/4] Generating embeddings for all chunks...")
print("  (This will take 1-2 minutes for 550 chunks)")

# The 'text' column is the cleaned, lowercase version
# Perfect for semantic understanding
texts = df['text'].tolist()

# Generate embeddings: text → 384-dimensional vector
# Each dimension captures some aspect of meaning
embeddings = model.encode(texts, show_progress_bar=True)

print(f"✓ Generated {len(embeddings)} embeddings")
print(f"  Each embedding is a {embeddings[0].shape[0]}-dimensional vector")

# -----------------------------------------------------------------------------
# STEP 4: Store in ChromaDB with COSINE similarity
# -----------------------------------------------------------------------------
print("\n[4/4] Storing in ChromaDB vector database...")
print("  Using COSINE similarity (better for text)")

# Create a persistent ChromaDB client (saves to disk)
chroma_client = chromadb.PersistentClient(path="./chroma_db")

# Delete existing collection if it exists (fresh start)
try:
    chroma_client.delete_collection("executive_orders")
    print("  (Deleted existing collection)")
except:
    pass

# Create new collection with COSINE similarity
# This is the key change - cosine measures angle between vectors
# rather than straight-line distance (Euclidean)
collection = chroma_client.create_collection(
    name="executive_orders",
    metadata={"hnsw:space": "cosine"}  # ← COSINE SIMILARITY
)

print("  Configured for COSINE similarity")

# Prepare data for ChromaDB
# ChromaDB needs: documents, embeddings, metadata, and IDs
ids = [f"chunk_{i}" for i in range(len(df))]
documents = df['text'].tolist()  # The cleaned text

# Metadata: all the info we want to retrieve later
metadatas = []
for _, row in df.iterrows():
    metadatas.append({
        'eo_number': str(row['eo_number']),
        'chunk_id': str(row['chunk_id']),
        'title': row['title'],
        'signing_date': str(row['signing_date']),
        'text_original': row['text_original'],  # Keep original for display
        'word_count': str(row['word_count'])
    })

# Add everything to ChromaDB in batches (more efficient)
batch_size = 100
for i in range(0, len(df), batch_size):
    end_idx = min(i + batch_size, len(df))
    
    collection.add(
        documents=documents[i:end_idx],
        embeddings=embeddings[i:end_idx].tolist(),
        metadatas=metadatas[i:end_idx],
        ids=ids[i:end_idx]
    )
    
    print(f"  Added batch {i//batch_size + 1} ({end_idx}/{len(df)} chunks)")

print("\n✓ Vector database created successfully!")
print(f"  Location: ./chroma_db/")
print(f"  Total chunks indexed: {len(df)}")
print(f"  Similarity metric: COSINE")

# -----------------------------------------------------------------------------
# STEP 5: Quick test with COSINE similarity
# -----------------------------------------------------------------------------
print("\n[Testing] Running a quick similarity search...")

test_query = "immigration policy"
query_embedding = model.encode([test_query])

results = collection.query(
    query_embeddings=query_embedding.tolist(),
    n_results=3
)

print(f"\nTest query: '{test_query}'")
print("Top 3 results (with COSINE similarity):")
print()

for i, (metadata, distance) in enumerate(zip(results['metadatas'][0], results['distances'][0])):
    # With cosine, ChromaDB returns distance = 1 - cosine_similarity
    # So we convert back: similarity = 1 - distance
    cosine_sim = 1 - distance
    
    print(f"  {i+1}. EO {metadata['eo_number']} - {metadata['title']}")
    print(f"     Cosine Similarity: {cosine_sim:.2%}")
    print(f"     Preview: {metadata['text_original'][:100]}...")
    print()

print("=" * 60)
print("DATABASE BUILD COMPLETE!")
print("=" * 60)
print("\nNote: Similarity scores now range 0-100%")
print("      Typical good matches: 40-70%")
print("      Excellent matches: 70%+")