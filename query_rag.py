"""
Step 2: Query the RAG System

This script demonstrates the full RAG pipeline:
1. Take a user query
2. Convert query to embedding
3. Find similar chunks in ChromaDB
4. Send chunks + query to LLM
5. Get natural language answer with citations
"""

import chromadb
from sentence_transformers import SentenceTransformer
import ollama

print("=" * 60)
print("EXECUTIVE ORDERS RAG QUERY SYSTEM")
print("=" * 60)

# -----------------------------------------------------------------------------
# STEP 1: Load the embedding model and database
# -----------------------------------------------------------------------------
print("\n[1/3] Loading resources...")

# Same model we used to build the database
model = SentenceTransformer('all-MiniLM-L6-v2')

# Connect to our ChromaDB
client = chromadb.PersistentClient(path="./chroma_db")
collection = client.get_collection("executive_orders")

print(f"✓ Loaded embedding model")
print(f"✓ Connected to database ({collection.count()} chunks)")

# -----------------------------------------------------------------------------
# STEP 2: Define the query function
# -----------------------------------------------------------------------------

def query_rag(question, n_results=5):
    """
    Main RAG pipeline:
    - Embed the question
    - Retrieve similar chunks
    - Generate answer using LLM
    """
    
    print(f"\n{'='*60}")
    print(f"QUERY: {question}")
    print(f"{'='*60}")
    
    # Step 2a: Convert question to embedding
    print("\n[2/3] Finding relevant chunks...")
    query_embedding = model.encode([question])
    
    # Step 2b: Search ChromaDB for similar chunks
    results = collection.query(
        query_embeddings=query_embedding.tolist(),
        n_results=n_results,
        include=['documents', 'metadatas', 'distances']
    )
    
    # Display what we found
    print(f"✓ Retrieved {len(results['metadatas'][0])} relevant chunks:\n")
    
    for i, metadata in enumerate(results['metadatas'][0]):
        distance = results['distances'][0][i]
        similarity = 1 - distance  # Convert distance to similarity
        print(f"  {i+1}. EO {metadata['eo_number']} - {metadata['title']}")
        print(f"     Similarity: {similarity:.2%}")
        print(f"     Preview: {metadata['text_original'][:80]}...\n")
    
    # Step 2c: Build context for LLM
    context = "\n\n---\n\n".join([
        f"[Source: EO {m['eo_number']}, Chunk {m['chunk_id']}]\n{m['text_original']}"
        for m in results['metadatas'][0]
    ])
    
    # Step 2d: Create prompt for LLM
    prompt = f"""You are a helpful assistant analyzing Executive Orders from President Trump's second term (January-July 2025).

Based on the following excerpts from Executive Orders, answer the question below. 

If the answer is in the provided excerpts, cite the specific Executive Order number(s) in your response.
If the answer is NOT in the excerpts, say so clearly - do not make up information.

EXECUTIVE ORDER EXCERPTS:
{context}

QUESTION: {question}

ANSWER:"""

    # Step 2e: Send to LLM
    print("[3/3] Generating answer with Llama 3.2...\n")
    
    response = ollama.generate(
        model='llama3.2',
        prompt=prompt,
        options={
            'temperature': 0.3,  # Lower = more focused/factual
            'num_predict': 500,  # Max tokens in response
        }
    )
    
    answer = response['response']
    
    # Display the answer
    print(f"{'='*60}")
    print("ANSWER:")
    print(f"{'='*60}")
    print(answer)
    print(f"\n{'='*60}\n")
    
    return {
        'question': question,
        'answer': answer,
        'sources': results['metadatas'][0]
    }

# -----------------------------------------------------------------------------
# STEP 3: Run some example queries
# -----------------------------------------------------------------------------

print("\n" + "=" * 60)
print("RUNNING EXAMPLE QUERIES")
print("=" * 60)

# Example 1: Factual question
query_rag("What Executive Orders address immigration policy?")

# Example 2: Analytical question  
query_rag("How does Trump approach federal regulations in these Executive Orders?")

# Example 3: Specific topic
query_rag("What actions were taken regarding energy policy?")

print("\n" + "=" * 60)
print("RAG QUERY SYSTEM TEST COMPLETE")
print("=" * 60)