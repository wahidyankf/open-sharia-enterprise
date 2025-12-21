#!/bin/bash

# Language Inventory Script for Programming Language Content Parity
# Generates CSV files with file counts, line counts, and weights for all 6 languages

# Base directory for programming language content
BASE_DIR="apps/ayokoding-web/content/en/learn/swe/prog-lang"

# Languages to analyze
LANGUAGES=("python" "golang" "java" "kotlin" "rust" "elixir")

# CSV output files
INVENTORY_CSV="plans/in-progress/2025-12-21__prog-lang-parity/analysis-inventory.csv"
QUALITY_CSV="plans/in-progress/2025-12-21__prog-lang-parity/analysis-quality-metrics.csv"

# Initialize CSV files with headers
echo "Language,Category,File,LineCount,Weight,Present" > "$INVENTORY_CSV"
echo "Language,Category,File,LineCount,DiagramCount,CodeBlockCount,LinkCount,ColorViolations,HasFrontHook,HasLearningPath,HasPrerequisites" > "$QUALITY_CSV"

# Function to extract weight from frontmatter
get_weight() {
    local file="$1"
    if [ -f "$file" ]; then
        grep "^weight:" "$file" | head -1 | sed 's/weight: *//' | tr -d '\r'
    else
        echo "N/A"
    fi
}

# Function to count lines in file
count_lines() {
    local file="$1"
    if [ -f "$file" ]; then
        wc -l < "$file" | tr -d ' '
    else
        echo "0"
    fi
}

# Function to count mermaid diagrams
count_diagrams() {
    local file="$1"
    if [ -f "$file" ]; then
        grep -c "^\`\`\`mermaid" "$file" 2>/dev/null || echo "0"
    else
        echo "0"
    fi
}

# Function to count code blocks
count_code_blocks() {
    local file="$1"
    if [ -f "$file" ]; then
        grep -c "^\`\`\`" "$file" 2>/dev/null || echo "0"
    else
        echo "0"
    fi
}

# Function to count cross-reference links
count_links() {
    local file="$1"
    if [ -f "$file" ]; then
        grep -c "](/" "$file" 2>/dev/null || echo "0"
    else
        echo "0"
    fi
}

# Function to check for color palette violations (red, green, yellow)
check_color_violations() {
    local file="$1"
    if [ -f "$file" ]; then
        grep -iE "#ff0000|#00ff00|#ffff00|'red'|'green'|'yellow'|\bred\b|\bgreen\b|\byellow\b" "$file" 2>/dev/null | wc -l | tr -d ' '
    else
        echo "0"
    fi
}

# Function to check for front hook (first paragraph engaging question/statement)
has_front_hook() {
    local file="$1"
    if [ -f "$file" ]; then
        # Check if content starts with engaging patterns after frontmatter
        if grep -A 20 "^---$" "$file" | tail -n +2 | grep -qE "^(Want to|Ever wondered|Imagine|What if|Have you)"; then
            echo "true"
        else
            echo "false"
        fi
    else
        echo "N/A"
    fi
}

# Function to check for learning path diagram
has_learning_path() {
    local file="$1"
    if [ -f "$file" ]; then
        if grep -q "learning path\|Learning Path\|LEARNING PATH" "$file"; then
            echo "true"
        else
            echo "false"
        fi
    else
        echo "N/A"
    fi
}

# Function to check for prerequisites section
has_prerequisites() {
    local file="$1"
    if [ -f "$file" ]; then
        if grep -q "## Prerequisites\|## prereq" "$file"; then
            echo "true"
        else
            echo "false"
        fi
    else
        echo "N/A"
    fi
}

# Process each language
for lang in "${LANGUAGES[@]}"; do
    echo "Processing $lang..."

    lang_dir="$BASE_DIR/$lang"

    # Check if language directory exists
    if [ ! -d "$lang_dir" ]; then
        echo "Warning: $lang_dir does not exist"
        continue
    fi

    # Process category folders
    for category in "tutorials" "how-to" "explanation" "reference"; do
        category_dir="$lang_dir/$category"

        # Category _index.md
        index_file="$category_dir/_index.md"
        if [ -f "$index_file" ]; then
            lines=$(count_lines "$index_file")
            weight=$(get_weight "$index_file")
            echo "$lang,$category,_index.md,$lines,$weight,true" >> "$INVENTORY_CSV"
        else
            echo "$lang,$category,_index.md,0,N/A,false" >> "$INVENTORY_CSV"
        fi

        # Category overview.md
        overview_file="$category_dir/overview.md"
        if [ -f "$overview_file" ]; then
            lines=$(count_lines "$overview_file")
            weight=$(get_weight "$overview_file")
            echo "$lang,$category,overview.md,$lines,$weight,true" >> "$INVENTORY_CSV"
        else
            echo "$lang,$category,overview.md,0,N/A,false" >> "$INVENTORY_CSV"
        fi
    done

    # Process tutorials
    tutorials_dir="$lang_dir/tutorials"
    for tutorial in "initial-setup" "quick-start" "beginner" "intermediate" "advanced"; do
        file="$tutorials_dir/${tutorial}.md"
        if [ -f "$file" ]; then
            lines=$(count_lines "$file")
            weight=$(get_weight "$file")
            diagrams=$(count_diagrams "$file")
            code_blocks=$(count_code_blocks "$file")
            links=$(count_links "$file")
            color_violations=$(check_color_violations "$file")
            front_hook=$(has_front_hook "$file")
            learning_path=$(has_learning_path "$file")
            prereqs=$(has_prerequisites "$file")

            echo "$lang,tutorials,${tutorial}.md,$lines,$weight,true" >> "$INVENTORY_CSV"
            echo "$lang,tutorials,${tutorial}.md,$lines,$diagrams,$code_blocks,$links,$color_violations,$front_hook,$learning_path,$prereqs" >> "$QUALITY_CSV"
        else
            echo "$lang,tutorials,${tutorial}.md,0,N/A,false" >> "$INVENTORY_CSV"
        fi
    done

    # Process how-to guides (including cookbook)
    howto_dir="$lang_dir/how-to"
    if [ -d "$howto_dir" ]; then
        # Cookbook
        cookbook_file="$howto_dir/cookbook.md"
        if [ -f "$cookbook_file" ]; then
            lines=$(count_lines "$cookbook_file")
            weight=$(get_weight "$cookbook_file")
            diagrams=$(count_diagrams "$cookbook_file")
            code_blocks=$(count_code_blocks "$cookbook_file")
            links=$(count_links "$cookbook_file")
            color_violations=$(check_color_violations "$cookbook_file")

            echo "$lang,how-to,cookbook.md,$lines,$weight,true" >> "$INVENTORY_CSV"
            echo "$lang,how-to,cookbook.md,$lines,$diagrams,$code_blocks,$links,$color_violations,N/A,N/A,N/A" >> "$QUALITY_CSV"
        else
            echo "$lang,how-to,cookbook.md,0,N/A,false" >> "$INVENTORY_CSV"
        fi

        # Count other how-to guides
        guide_count=$(find "$howto_dir" -maxdepth 1 -name "*.md" ! -name "_index.md" ! -name "overview.md" ! -name "cookbook.md" | wc -l | tr -d ' ')
        echo "$lang,how-to,guide-count,$guide_count,N/A,true" >> "$INVENTORY_CSV"
    fi

    # Process explanation files
    explanation_dir="$lang_dir/explanation"
    for expl in "best-practices" "anti-patterns"; do
        file="$explanation_dir/${expl}.md"
        if [ -f "$file" ]; then
            lines=$(count_lines "$file")
            weight=$(get_weight "$file")
            diagrams=$(count_diagrams "$file")
            code_blocks=$(count_code_blocks "$file")
            links=$(count_links "$file")
            color_violations=$(check_color_violations "$file")

            echo "$lang,explanation,${expl}.md,$lines,$weight,true" >> "$INVENTORY_CSV"
            echo "$lang,explanation,${expl}.md,$lines,$diagrams,$code_blocks,$links,$color_violations,N/A,N/A,N/A" >> "$QUALITY_CSV"
        else
            echo "$lang,explanation,${expl}.md,0,N/A,false" >> "$INVENTORY_CSV"
        fi
    done

    # Process reference files
    reference_dir="$lang_dir/reference"
    for ref in "cheat-sheet" "glossary" "resources"; do
        file="$reference_dir/${ref}.md"
        if [ -f "$file" ]; then
            lines=$(count_lines "$file")
            weight=$(get_weight "$file")
            diagrams=$(count_diagrams "$file")
            code_blocks=$(count_code_blocks "$file")
            links=$(count_links "$file")
            color_violations=$(check_color_violations "$file")

            echo "$lang,reference,${ref}.md,$lines,$weight,true" >> "$INVENTORY_CSV"
            echo "$lang,reference,${ref}.md,$lines,$diagrams,$code_blocks,$links,$color_violations,N/A,N/A,N/A" >> "$QUALITY_CSV"
        else
            echo "$lang,reference,${ref}.md,0,N/A,false" >> "$INVENTORY_CSV"
        fi
    done
done

echo ""
echo "Inventory complete!"
echo "Results saved to:"
echo "  - $INVENTORY_CSV"
echo "  - $QUALITY_CSV"
echo ""
echo "Summary:"
wc -l "$INVENTORY_CSV" "$QUALITY_CSV"
