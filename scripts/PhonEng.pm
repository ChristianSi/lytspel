# Shared code for the phoneng scripts.
#
# Copyright (c) 2016-2019 Christian Siefkes
# See accompanying LICENSE.txt file for licensing information.
#
# Required modules: Text::CSV_XS.

package PhonEng;

use 5.014;
use open qw(:std :utf8);  # use UTF-8 for all I/O (by default)
use utf8;                 # allow UTF-8 in source code
use warnings;

use Exporter 'import';
use Text::CSV_XS;

our @EXPORT = qw(
    build_lc_map
    gen_key
    new_csv_in
    new_csv_out
    open_outfile_and_write_header
    rename_to_backup_file_if_exists
    valid_key
    valid_pos_tag
    write_csv_line
);

# new_csv_in: Create an instance of Text::CSV_XS suitable for reading.
# Any common line separators are accepted.
sub new_csv_in {
    return Text::CSV_XS->new({auto_diag => 1});
}

# new_csv_out: Create an instance of Text::CSV_XS suitable for writing.
# Lines are separated by the usual line separator of the used platform.
sub new_csv_out : {
    return Text::CSV_XS->new({eol => "\n", auto_diag => 1});
}

# File-scoped to allow re-use
my $Csv_In  = new_csv_in;
my $Csv_Out = new_csv_out;

# gen_key $word, $pos: Combine a word and a optional POS tag into a joint key.
# If $pos is undefined or empty, $word alone is returned, otherwise "$word/$pos" is returned.
sub gen_key {
    my ($word, $pos) = @_;
    return $pos ? "$word/$pos" : $word;
}

# valid_pos_tag $tag: Check whether the argument is a valid POS tag.
sub valid_pos_tag {
    my $tag = shift;
    return $tag =~ /^(aj|av|n|prp|v)$/;
}

# build_lc_map $filename, $pos_tagged, $valueless: Create and return a mapping from a simple
# CSV file containing up to three columns (keys and values). If $pos_tagged is true, the 2nd column
# contains an optional POS tag which becomes part of the key per the 'gen_key' function.
# Unless $valueless is true, the final (second or third) column contains the value; otherwise,
# all values are set to 1.
#
# All keys are converted to lower-case, while values are used as is. Warns if there are
# duplicate keys.
sub build_lc_map {
    my ($filename, $pos_tagged, $valueless) = @_;
    my %dict;
    open my $fh, '<', $filename or die "Unable to open $filename: $!\n";
    my $colref = $Csv_In->getline($fh);  # Skip header line

    while ($colref = $Csv_In->getline($fh)) {
        my $key = lc($colref->[0]);
        my $value;

        if ($pos_tagged) {
            my $tag = $colref->[1];
            die "Invalid POS tag '$tag' for entry '$key' in $filename\n"
                if $tag && !valid_pos_tag($tag);
            $key = gen_key($key, $tag);
            $value = $valueless ? 1 : $colref->[2];
        } else {
            $value = $valueless ? 1 : $colref->[1];
        }

        warn "Duplicate key '$key' in $filename\n" if exists($dict{$key});
        die "Valueless key '$key' in $filename\n" unless $value;
        $dict{$key} = $value;
    }

    close $fh;
    return \%dict;
}

# rename_to_backup_file_if_exists $filename: Rename a file into a backup
# file by appending '.bak' to its name. A previously existing backup file
# with the same name will be silently overwritten. If $filename doesn't exist,
# this function does nothing.
sub rename_to_backup_file_if_exists {
    my ($filename) = @_;
    rename $filename, "$filename.bak" if -e $filename;
}

# open_outfile_and_write_header $filename, $headers: Open $filename for writing and writes
# $headers (an array reference) as header line in CSV format.
# Returns the opened file handle. Also backups an old version of the file, if any.
sub open_outfile_and_write_header {
    my ($filename, $headers) = @_;
    rename_to_backup_file_if_exists $filename;
    open my $outfh, '>', $filename or die "Unable to open $filename for writing: $!\n";
    $Csv_Out->print($outfh, $headers);
    return $outfh;
}

# valid_key $key: Check whether a mapping key is valid. Empty or undefined keys are invalid;
# '-' is invalid too (it marks keys that should be skipped).
sub valid_key {
    my $key = shift;
    return $key && $key ne '-';
}

1;
