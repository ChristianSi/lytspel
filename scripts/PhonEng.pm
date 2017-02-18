# Shared code for the phoneng scripts.
#
# Copyright (c) 2016-17 Christian Siefkes
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
    new_csv_in
    new_csv_out
    write_csv_line
    rename_to_backup_file_if_exists
    open_outfile_and_write_header
    valid_key
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

my $CSV_OUT = new_csv_out;

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
    $CSV_OUT->print($outfh, $headers);
    return $outfh;
}

# valid_key $key: Check whether a mapping key is valid. Empty or undefined keys are invalid;
# '-' is invalid too (it marks keys that should be skipped).
sub valid_key {
    my $key = shift;
    return $key && $key ne '-';
}

1;
