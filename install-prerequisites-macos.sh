#!/usr/bin/env bash
set -euo pipefail

# Homebrew
if ! command -v brew >/dev/null 2>&1; then
  echo "Homebrew is required. Install it from https://brew.sh"
  exit 1
fi

# Quarto
if command -v quarto >/dev/null 2>&1; then
  echo "Quarto is already installed: $(quarto --version)"
else
  echo "Installing Quarto..."
  brew install --cask quarto
fi

# TinyTeX
if quarto list tools 2>/dev/null |
  grep -Eiq '^tinytex[[:space:]].*(installed|external installation)'; then
  echo "TinyTeX or another recognized TinyTeX installation is already available."
else
  echo "Installing TinyTeX..."
  quarto install tinytex --no-prompt
fi

# VS Code extensions
if command -v code >/dev/null 2>&1; then
  for extension in quarto.quarto REditorSupport.r; do
    if code --list-extensions |
      grep -Fxiq "$extension"; then
      echo "VS Code extension already installed: $extension"
    else
      echo "Installing VS Code extension: $extension"
      code --install-extension "$extension"
    fi
  done
else
  echo "VS Code 'code' command not found; skipping extensions."
fi

# R
if command -v Rscript >/dev/null 2>&1; then
  echo "R is already installed: $(Rscript --version 2>&1)"
else
  echo "WARNING: R is not installed."
  echo "Install it with: brew install --cask r"
fi

echo
echo "Checking Quarto..."
quarto check
