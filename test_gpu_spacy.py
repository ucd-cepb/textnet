#!/usr/bin/env python
"""
Test script for GPU-enabled spaCy transformer model.
Run from command line:
    export CUDA_PATH=/cvmfs/hpc.ucdavis.edu/sw/spack/environments/core/view/generic/cuda-12.3.0
    /home/tscott1/.conda/envs/textnet/bin/python test_gpu_spacy.py
"""

import os
import sys

print("=" * 60)
print("GPU spaCy Transformer Test")
print("=" * 60)

# Step 1: Set CUDA_PATH if not set
if 'CUDA_PATH' not in os.environ:
    if 'CUDA_HOME' in os.environ:
        os.environ['CUDA_PATH'] = os.environ['CUDA_HOME']
        print(f"Set CUDA_PATH from CUDA_HOME: {os.environ['CUDA_PATH']}")
    else:
        print("WARNING: CUDA_PATH and CUDA_HOME not set")
else:
    print(f"CUDA_PATH: {os.environ['CUDA_PATH']}")

# Step 2: Import and test cupy
print("\n--- Testing cupy ---")
try:
    import cupy
    device_count = cupy.cuda.runtime.getDeviceCount()
    print(f"cupy version: {cupy.__version__}")
    print(f"GPU device count: {device_count}")

    if device_count == 0:
        print("ERROR: No GPU detected by cupy")
        sys.exit(1)

    cupy.cuda.Device(0).use()
    print("GPU device 0 activated")
except Exception as e:
    print(f"ERROR with cupy: {e}")
    sys.exit(1)

# Step 3: Fix thinc's cupy detection
print("\n--- Fixing thinc cupy detection ---")
try:
    import thinc.util
    print(f"thinc.util.has_cupy before fix: {thinc.util.has_cupy}")

    thinc.util.cupy = cupy
    thinc.util.has_cupy = True
    thinc.util.has_cupy_gpu = True
    thinc.util.has_gpu = True

    print(f"thinc.util.has_cupy after fix: {thinc.util.has_cupy}")
except Exception as e:
    print(f"ERROR fixing thinc: {e}")
    sys.exit(1)

# Step 4: Import spacy and enable GPU
print("\n--- Enabling spaCy GPU ---")
try:
    import spacy
    result = spacy.prefer_gpu(0)
    print(f"spacy.prefer_gpu(0) returned: {result}")

    if not result:
        print("WARNING: prefer_gpu returned False")
except Exception as e:
    print(f"ERROR with spacy GPU: {e}")
    sys.exit(1)

# Step 5: Verify ops
print("\n--- Verifying ops backend ---")
try:
    from thinc.api import get_current_ops
    ops = get_current_ops()
    print(f"Current ops type: {type(ops).__name__}")

    if ops is None:
        print("ERROR: ops is None")
        sys.exit(1)
except Exception as e:
    print(f"ERROR checking ops: {e}")
    sys.exit(1)

# Step 6: Load the transformer model
print("\n--- Loading en_core_web_trf model ---")
print("(This may take a moment...)")
try:
    nlp = spacy.load("en_core_web_trf")
    print("SUCCESS: Model loaded!")
except Exception as e:
    print(f"ERROR loading model: {e}")
    import traceback
    traceback.print_exc()
    sys.exit(1)

# Step 7: Test the model
print("\n--- Testing model ---")
try:
    doc = nlp("This is a test sentence about California and climate change.")
    print(f"Tokens: {[token.text for token in doc]}")
    print(f"Entities: {[(ent.text, ent.label_) for ent in doc.ents]}")
    print("\nSUCCESS: Model working correctly!")
except Exception as e:
    print(f"ERROR testing model: {e}")
    sys.exit(1)

print("\n" + "=" * 60)
print("ALL TESTS PASSED")
print("=" * 60)
