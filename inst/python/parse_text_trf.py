"""
parse_text_trf.py - Pure Python spaCy parsing with transformer model

This module provides text parsing using spaCy's en_core_web_trf transformer model.
It is designed to be called from R via reticulate, bypassing spacyr to avoid
GPU initialization issues.

Output format matches spacyr::spacy_parse() for compatibility with existing textNet code.
"""

import os
import json
import spacy
import pandas as pd
from typing import List, Dict, Optional, Any
from pathlib import Path


class SpacyParserTRF:
    """SpaCy parser using transformer model with optional EntityRuler support."""

    def __init__(self, model: str = "en_core_web_trf"):
        """
        Initialize the parser with the specified model.

        Args:
            model: spaCy model name (default: en_core_web_trf)
        """
        self.model_name = model
        self.nlp = None
        self._initialized = False

    def initialize(
        self,
        entity_ruler_patterns: Optional[List[Dict]] = None,
        ruler_position: str = "after",
        overwrite_ents: bool = True
    ) -> None:
        """
        Load the spaCy model and configure pipeline.

        Args:
            entity_ruler_patterns: List of pattern dicts with 'label' and 'pattern' keys
            ruler_position: "after" or "before" NER in pipeline
            overwrite_ents: Whether EntityRuler overrides NER assignments
        """
        # Activate GPU before loading model — uses MPS on Apple Silicon, CUDA on NVIDIA
        gpu_activated = spacy.prefer_gpu()
        if gpu_activated:
            from thinc.api import get_current_ops
            ops = get_current_ops()
            print(f"GPU activated: {type(ops).__name__}")
        else:
            print("No GPU available, using CPU")

        print(f"Loading spaCy model: {self.model_name}")
        self.nlp = spacy.load(self.model_name)
        print(f"Model loaded: {self.nlp.meta['name']}")

        # Configure EntityRuler if patterns provided
        if entity_ruler_patterns:
            self._add_entity_ruler(entity_ruler_patterns, ruler_position, overwrite_ents)

        self._initialized = True

    def _add_entity_ruler(
        self,
        patterns: List[Dict],
        position: str,
        overwrite_ents: bool
    ) -> None:
        """Add EntityRuler to the pipeline."""
        config = {"overwrite_ents": overwrite_ents}

        if position == "after":
            ruler = self.nlp.add_pipe("entity_ruler", after="ner", config=config)
        else:
            ruler = self.nlp.add_pipe("entity_ruler", before="ner", config=config)

        ruler.add_patterns(patterns)
        print(f"EntityRuler configured: position={position}, overwrite_ents={overwrite_ents}, {len(patterns)} patterns")

    def parse_texts(
        self,
        texts: List[str],
        doc_ids: List[str],
        batch_size: int = 50
    ) -> pd.DataFrame:
        """
        Parse a list of texts and return results in spacyr-compatible format.

        Args:
            texts: List of text strings to parse
            doc_ids: List of document IDs corresponding to each text
            batch_size: Batch size for nlp.pipe()

        Returns:
            DataFrame with columns: doc_id, sentence_id, token_id, token, lemma,
                                   pos, tag, entity, head_token_id, dep_rel
        """
        if not self._initialized:
            raise RuntimeError("Parser not initialized. Call initialize() first.")

        if len(texts) != len(doc_ids):
            raise ValueError("texts and doc_ids must have same length")

        all_rows = []

        # Process with nlp.pipe for efficiency
        docs = list(self.nlp.pipe(texts, batch_size=batch_size))

        for doc, doc_id in zip(docs, doc_ids):
            doc_rows = self._parse_single_doc(doc, doc_id)
            all_rows.extend(doc_rows)

        df = pd.DataFrame(all_rows)

        # Ensure correct column order matching spacyr output
        columns = ['doc_id', 'sentence_id', 'token_id', 'token', 'lemma',
                   'pos', 'tag', 'entity', 'head_token_id', 'dep_rel']

        return df[columns]

    def _parse_single_doc(self, doc, doc_id: str) -> List[Dict]:
        """Parse a single spaCy Doc into rows."""
        rows = []

        for sent_idx, sent in enumerate(doc.sents, start=1):
            # Build token index map for this sentence (for head references)
            sent_token_ids = {token.i: idx for idx, token in enumerate(sent, start=1)}

            for token_idx, token in enumerate(sent, start=1):
                # Get entity IOB tag (e.g., "PERSON_B", "ORG_I", or "")
                if token.ent_type_:
                    entity = f"{token.ent_type_}_{token.ent_iob_}"
                else:
                    entity = ""

                # Get head token ID within sentence
                # If head is outside sentence or is self (ROOT), use token's own ID
                if token.head.i in sent_token_ids:
                    head_token_id = sent_token_ids[token.head.i]
                else:
                    head_token_id = token_idx

                rows.append({
                    'doc_id': doc_id,
                    'sentence_id': sent_idx,
                    'token_id': token_idx,
                    'token': token.text,
                    'lemma': token.lemma_,
                    'pos': token.pos_,
                    'tag': token.tag_,
                    'entity': entity,
                    'head_token_id': head_token_id,
                    'dep_rel': token.dep_
                })

        return rows

    def finalize(self) -> None:
        """Clean up resources."""
        if self.nlp is not None:
            del self.nlp
            self.nlp = None
        self._initialized = False

        # Clean up GPU memory if using PyTorch
        try:
            import torch
            if torch.cuda.is_available():
                torch.cuda.empty_cache()
            elif hasattr(torch.backends, 'mps') and torch.backends.mps.is_available():
                # MPS doesn't have empty_cache, but releasing tensors via gc is sufficient
                pass
        except ImportError:
            pass

        import gc
        gc.collect()
        print("Parser finalized and resources cleaned up")


# Module-level parser instance for use from R
_parser = None


def initialize(
    model: str = "en_core_web_trf",
    entity_ruler_patterns: Optional[List[Dict]] = None,
    ruler_position: str = "after",
    overwrite_ents: bool = True
) -> None:
    """
    Initialize the global parser instance.

    Called from R via reticulate.
    """
    global _parser

    if _parser is not None:
        _parser.finalize()

    _parser = SpacyParserTRF(model=model)
    _parser.initialize(
        entity_ruler_patterns=entity_ruler_patterns,
        ruler_position=ruler_position,
        overwrite_ents=overwrite_ents
    )


def parse_and_save(
    texts: List[str],
    doc_ids: List[str],
    output_path: str,
    batch_size: int = 50
) -> str:
    """
    Parse texts and save to Parquet file.

    Args:
        texts: List of text strings
        doc_ids: List of document IDs
        output_path: Path for output Parquet file
        batch_size: Batch size for processing

    Returns:
        Path to the saved file
    """
    global _parser

    if _parser is None:
        raise RuntimeError("Parser not initialized. Call initialize() first.")

    df = _parser.parse_texts(texts, doc_ids, batch_size=batch_size)

    # Ensure output directory exists
    Path(output_path).parent.mkdir(parents=True, exist_ok=True)

    # Save to Parquet
    df.to_parquet(output_path, index=False)
    print(f"Saved {len(df)} tokens to {output_path}")

    return output_path


def parse_to_dataframe(
    texts: List[str],
    doc_ids: List[str],
    batch_size: int = 50
) -> Dict[str, list]:
    """
    Parse texts and return as dict of lists (converts cleanly to R data.frame).

    Args:
        texts: List of text strings
        doc_ids: List of document IDs
        batch_size: Batch size for processing

    Returns:
        Dict of lists with keys: doc_id, sentence_id, token_id, token, lemma,
                                 pos, tag, entity, head_token_id, dep_rel
    """
    global _parser

    if _parser is None:
        raise RuntimeError("Parser not initialized. Call initialize() first.")

    df = _parser.parse_texts(texts, doc_ids, batch_size=batch_size)
    # Convert to dict of plain Python lists for clean reticulate conversion
    return {col: df[col].tolist() for col in df.columns}


def finalize() -> None:
    """Clean up the global parser instance."""
    global _parser

    if _parser is not None:
        _parser.finalize()
        _parser = None


def apply_custom_entities(
    df: pd.DataFrame,
    custom_entities: Dict[str, List[str]],
    concatenator: str = "_"
) -> pd.DataFrame:
    """
    Apply custom entity labels to tokens that match specified patterns.

    This replicates the custom_entities functionality from the R code.
    Only applies to tokens that don't already have an entity label.

    Args:
        df: DataFrame with parsed tokens
        custom_entities: Dict mapping entity labels to lists of token strings
        concatenator: Character used to replace spaces in patterns

    Returns:
        DataFrame with updated entity column
    """
    df = df.copy()

    for entity_label, patterns in custom_entities.items():
        # Replace spaces with concatenator in patterns
        patterns = [p.replace(" ", concatenator) for p in patterns]

        # Find tokens that match patterns and have no entity
        mask = (df['token'].isin(patterns)) & (df['entity'] == "")

        if mask.any():
            df.loc[mask, 'entity'] = f"{entity_label}_B"

    return df
