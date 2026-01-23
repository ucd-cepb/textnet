#!/usr/bin/env python3
"""
Standalone GPU parser for spaCy transformer models.
Called by textNet::parse_text_trf() to avoid reticulate/thinc detection issues.

Usage:
    python parse_gpu.py --input input.json --output output.json [--entity-ruler patterns.json]
"""

import argparse
import json
import sys
import os

def initialize_gpu():
    """Initialize GPU for spaCy transformer model."""
    # Set CUDA path if available
    cuda_path = os.environ.get('CUDA_PATH') or os.environ.get('CUDA_HOME')
    if cuda_path:
        os.environ['CUDA_PATH'] = cuda_path
        print(f'CUDA_PATH: {cuda_path}', file=sys.stderr)

    # Import and verify cupy
    try:
        import cupy
        device_count = cupy.cuda.runtime.getDeviceCount()
        if device_count == 0:
            raise RuntimeError('No GPU detected by cupy')
        cupy.cuda.Device(0).use()
        print(f'cupy: {device_count} GPU(s) detected', file=sys.stderr)
    except ImportError:
        raise RuntimeError('cupy is not installed')

    # Import spacy and enable GPU
    import spacy
    if spacy.prefer_gpu(0):
        print('spaCy GPU enabled', file=sys.stderr)
    else:
        raise RuntimeError('spacy.prefer_gpu() failed')

    return spacy

def load_model(spacy, model_name='en_core_web_trf'):
    """Load the spaCy model."""
    print(f'Loading {model_name}...', file=sys.stderr)
    nlp = spacy.load(model_name)
    print('Model loaded successfully', file=sys.stderr)
    return nlp

def add_entity_ruler(nlp, patterns, position='after', overwrite_ents=True):
    """Add EntityRuler to the pipeline."""
    if not patterns:
        return

    config = {'overwrite_ents': overwrite_ents}

    if position == 'after':
        ruler = nlp.add_pipe('entity_ruler', after='ner', config=config)
    else:
        ruler = nlp.add_pipe('entity_ruler', before='ner', config=config)

    ruler.add_patterns(patterns)
    print(f'EntityRuler added: {len(patterns)} patterns, position={position}', file=sys.stderr)

def parse_texts(nlp, texts, doc_ids):
    """Parse texts and return spacyr-compatible format."""
    all_tokens = []
    all_nounphrases = []

    for text, doc_id in zip(texts, doc_ids):
        if not text or not text.strip():
            continue

        doc = nlp(text)

        # Process by sentence to get sentence-relative token IDs (spacyr format)
        for sent_i, sent in enumerate(doc.sents):
            sent_start = sent.start

            for token in sent:
                ent_tag = ''
                if token.ent_type_:
                    ent_tag = token.ent_type_ + '_' + token.ent_iob_

                # Sentence-relative token_id (1-indexed)
                token_id_rel = token.i - sent_start + 1

                # Head token_id is also sentence-relative
                if token.head.i >= sent.start and token.head.i < sent.end:
                    head_token_id_rel = token.head.i - sent_start + 1
                else:
                    head_token_id_rel = token_id_rel

                all_tokens.append({
                    'doc_id': doc_id,
                    'sentence_id': sent_i + 1,
                    'token_id': token_id_rel,
                    'token': token.text,
                    'lemma': token.lemma_,
                    'pos': token.pos_,
                    'tag': token.tag_,
                    'head_token_id': head_token_id_rel,
                    'dep_rel': token.dep_,
                    'entity': ent_tag
                })

            # Extract noun phrases for this sentence
            for chunk in doc.noun_chunks:
                if chunk.start >= sent.start and chunk.start < sent.end:
                    all_nounphrases.append({
                        'doc_id': doc_id,
                        'sentence_id': sent_i + 1,
                        'nounphrase': chunk.text,
                        'root': chunk.root.text
                    })

    return {'tokens': all_tokens, 'nounphrases': all_nounphrases}

def main():
    parser = argparse.ArgumentParser(description='GPU parser for spaCy transformer models')
    parser.add_argument('--input', required=True, help='Input JSON file with texts and doc_ids')
    parser.add_argument('--output', required=True, help='Output JSON file for parsed results')
    parser.add_argument('--entity-ruler', help='Optional JSON file with EntityRuler patterns')
    parser.add_argument('--ruler-position', default='after', choices=['after', 'before'])
    parser.add_argument('--overwrite-ents', action='store_true', default=True)
    parser.add_argument('--model', default='en_core_web_trf', help='spaCy model name')
    args = parser.parse_args()

    # Read input
    with open(args.input, 'r', encoding='utf-8') as f:
        input_data = json.load(f)

    texts = input_data['texts']
    doc_ids = input_data['doc_ids']

    # Initialize GPU and load model
    spacy = initialize_gpu()
    nlp = load_model(spacy, args.model)

    # Add EntityRuler if patterns provided
    if args.entity_ruler:
        with open(args.entity_ruler, 'r', encoding='utf-8') as f:
            patterns = json.load(f)
        add_entity_ruler(nlp, patterns, args.ruler_position, args.overwrite_ents)

    # Parse texts
    print(f'Parsing {len(texts)} text(s)...', file=sys.stderr)
    result = parse_texts(nlp, texts, doc_ids)
    print(f'Parsed {len(result["tokens"])} tokens, {len(result["nounphrases"])} noun phrases', file=sys.stderr)

    # Write output
    with open(args.output, 'w', encoding='utf-8') as f:
        json.dump(result, f, ensure_ascii=False)

    print(f'Output written to {args.output}', file=sys.stderr)

if __name__ == '__main__':
    main()
