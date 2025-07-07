
# 📘 Project: Spellchecker in Haskell

## 🔧 Overview
This Haskell project implements a simple spellchecker. It reads an input text file, compares the words against a dictionary, and outputs the corrected or marked words. It’s structured into multiple modules, following a clean and functional programming style.

## 📁 Project Structure

- `src/CommandLine.hs`: Handles command-line argument parsing.
- `src/Dictionary.hs`: Loads and manages the dictionary of valid words.
- `src/Document.hs`: Processes the input document.
- `src/SpellChecker.hs`: Core logic for detecting and correcting misspelled words.
- `src/Main.hs`: Entry point that ties all components together.

## 📄 Module Summaries

### 1. `CommandLine.hs`
Handles parsing command-line arguments:
- Defines a `Config` data type with fields for dictionary, input, and output files.
- Uses `System.Console.GetOpt` to define and parse options.
- Provides `parseArgs :: [String] -> IO Config`.

### 2. `Dictionary.hs`
Manages the dictionary of valid words:
- Provides `loadDict :: FilePath -> IO [String]`.
- Normalizes and sorts the list for efficient lookup.
- Assumes one word per line in the dictionary file.

### 3. `Document.hs`
Processes input documents:
- Loads input lines from a file: `loadLines :: FilePath -> IO [String]`.
- Outputs processed lines: `saveLines :: FilePath -> [String] -> IO ()`.

### 4. `SpellChecker.hs`
Core of the spellchecking system:
- `spellCheck :: [String] -> [String] -> [String]`:
  - Takes a dictionary and list of input lines.
  - Returns corrected or flagged lines.
- Uses word tokenization and linear search.
- Misspelled words are marked with `?`.

### 5. `Main.hs`
Program entry point:
- Loads config, dictionary, and input.
- Runs the spellcheck.
- Saves the result to output.

## 🚀 How to Use

### 1. Compile

```bash
ghc -o spellchk src/Main.hs
```

### 2. Run

```bash
./spellchk --dict dict.txt --input in.txt --output out.txt
```

- `dict.txt`: A file with one word per line.
- `in.txt`: Text to be checked.
- `out.txt`: Where the results are written.

## 📝 Example

**Input (`in.txt`):**
```
This is a smaple txt.
```

**Dictionary (`dict.txt`):**
```
this
is
a
sample
text
```

**Output (`out.txt`):**
```
This is a ?smaple ?txt.
```

## 🧩 Potential Improvements

- Add suggestions using edit distance.
- Support multi-language dictionaries.
- Handle punctuation more precisely.
- Use trie for dictionary lookup instead of linear search.
