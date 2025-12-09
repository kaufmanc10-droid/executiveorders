"""
Visualization utilities for the RAG system
"""

import plotly.graph_objects as go
import plotly.express as px
import pandas as pd
from collections import Counter

def plot_similarity_scores(results, query):
    """
    Bar chart of similarity scores for retrieved chunks
    """
    metadatas = results['metadatas'][0]
    distances = results['distances'][0]
    similarities = [(1 - d) * 100 for d in distances]
    
    # Create labels
    labels = [f"EO {m['eo_number']}\nChunk {m['chunk_id']}" for m in metadatas]
    
    fig = go.Figure(data=[
        go.Bar(
            x=labels,
            y=similarities,
            marker_color=similarities,
            marker_colorscale='Blues',
            text=[f"{s:.1f}%" for s in similarities],
            textposition='outside',
        )
    ])
    
    fig.update_layout(
        title=f"Similarity Scores for Query: '{query}'",
        xaxis_title="Executive Order Chunk",
        yaxis_title="Cosine Similarity (%)",
        yaxis_range=[0, 100],
        height=400,
        showlegend=False
    )
    
    return fig

def plot_eo_coverage(results):
    """
    Pie chart showing which EOs are represented in results
    """
    metadatas = results['metadatas'][0]
    eo_counts = Counter([m['eo_number'] for m in metadatas])
    
    fig = go.Figure(data=[
        go.Pie(
            labels=[f"EO {eo}" for eo in eo_counts.keys()],
            values=list(eo_counts.values()),
            hole=0.3
        )
    ])
    
    fig.update_layout(
        title="Executive Orders Represented in Results",
        height=400
    )
    
    return fig

def plot_temporal_distribution(results):
    """
    Timeline showing when relevant EOs were signed
    """
    metadatas = results['metadatas'][0]
    distances = results['distances'][0]
    
    df = pd.DataFrame([
        {
            'EO': m['eo_number'],
            'Date': pd.to_datetime(m['signing_date']),
            'Similarity': (1 - d) * 100,
            'Title': m['title'][:50] + "..." if len(m['title']) > 50 else m['title']
        }
        for m, d in zip(metadatas, distances)
    ])
    
    fig = px.scatter(
        df, 
        x='Date', 
        y='Similarity',
        size='Similarity',
        color='Similarity',
        hover_data=['EO', 'Title'],
        color_continuous_scale='Blues'
    )
    
    fig.update_layout(
        title="Temporal Distribution of Relevant Executive Orders",
        xaxis_title="Signing Date",
        yaxis_title="Cosine Similarity (%)",
        height=400
    )
    
    return fig

def plot_similarity_heatmap(query_results_dict):
    """
    Heatmap comparing similarity scores across multiple queries
    Shows which EOs are relevant to which topics
    """
    # query_results_dict = {"Query 1": results, "Query 2": results, ...}
    
    data = []
    for query, results in query_results_dict.items():
        metadatas = results['metadatas'][0]
        distances = results['distances'][0]
        
        for m, d in zip(metadatas, distances):
            data.append({
                'Query': query[:30] + "..." if len(query) > 30 else query,
                'EO': f"EO {m['eo_number']}",
                'Similarity': (1 - d) * 100
            })
    
    df = pd.DataFrame(data)
    pivot = df.pivot_table(values='Similarity', index='EO', columns='Query', fill_value=0)
    
    fig = go.Figure(data=go.Heatmap(
        z=pivot.values,
        x=pivot.columns,
        y=pivot.index,
        colorscale='Blues',
        text=[[f"{val:.1f}%" for val in row] for row in pivot.values],
        texttemplate="%{text}",
        textfont={"size": 10}
    ))
    
    fig.update_layout(
        title="Cross-Query Similarity Heatmap",
        xaxis_title="Query",
        yaxis_title="Executive Order",
        height=max(400, len(pivot) * 30)
    )
    
    return fig