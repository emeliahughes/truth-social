def fix_tsv_columns(input_file, output_file, expected_columns=13):
    """
    Fix rows in a TSV file by removing the 11th column (if it contains text) and the 14th column (if present),
    and ensuring rows conform to the expected number of columns.

    Parameters:
        input_file (str): Path to the input TSV file.
        output_file (str): Path to the output TSV file with fixed rows.
        expected_columns (int): Number of expected columns in each row. Defaults to 13.
    """
    def is_text(value):
        """Check if the value is text (non-numeric)."""
        try:
            float(value)  # Try converting to float (numeric)
            return False  # If conversion succeeds, it's a number
        except ValueError:
            return True  # If conversion fails, it's text (non-numeric)

    with open(input_file, 'r', encoding='utf-8', errors='replace') as infile, \
         open(output_file, 'w', encoding='utf-8') as outfile:

        first_row = True  # Flag to indicate the first row

        for line in infile:
            # Split the line into columns manually using tab as the delimiter
            row = line.strip().split('\t')

            # Skip deletion for the first row (header row)
            if first_row:
                outfile.write('\t'.join(row) + '\n')
                first_row = False
                continue

            # Remove the 14th column if it exists
            if len(row) >= 14:
                del row[13]  # Remove the 14th column (index 13)

            # Remove the 11th column if it contains text
            if len(row) >= 11 and is_text(row[10]):
                del row[10]  # Remove the 11th column (index 10)

            # Ensure the row has exactly the expected number of columns
            if len(row) < expected_columns:
                row.extend([''] * (expected_columns - len(row)))  # Add empty strings to pad

            # Trim the row to the expected number of columns if it exceeds the limit
            if len(row) > expected_columns:
                row = row[:expected_columns]

            # Write the cleaned row to the output file as a tab-separated line
            outfile.write('\t'.join(row) + '\n')

# Example usage
fix_tsv_columns('truths.tsv', 'truths-cleaned.tsv')

import csv

input_file = "truths-cleaned.tsv"
output_file = "bad_truths.tsv"

with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
    for line in infile:
        # Split the line by the tab delimiter
        columns = line.strip().split('\t')
        
        # Check if the row has more than 13 columns
        if len(columns) > 13:
            # Write the row to the output file
            outfile.write(line)
