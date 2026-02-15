#!/usr/bin/env bash

EMACS_DIR="$HOME/.config/emacs"
OUTPUT_FILE="keybind-list.txt"

echo "Extracting Emacs keybindings..."
echo "Output -> $OUTPUT_FILE"

{
echo "======================================"
echo "Emacs Custom Keybindings"
echo "Generated on: $(date)"
echo "======================================"
echo ""

grep -RhiE \
  --exclude="*.elc" \
  --exclude="*~" \
  --exclude-dir="elpa" \
  "global-set-key|define-key|keymap-set|:bind" \
  "$EMACS_DIR"

} > "$OUTPUT_FILE"

echo "Done ✅"
