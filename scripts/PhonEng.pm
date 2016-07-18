# Shared code for the phoneng scripts.
#
# Copyright (c) 2015 Christian Siefkes
# See accompanying LICENSE file for licensing information.
#
# Required modules: Text::CSV_XS.

package PhonEng;

use 5.014;
use open qw(:std :utf8);  # use UTF-8 for all I/O (by default)
use utf8;                 # allow UTF-8 in source code
use warnings;

use Exporter 'import';
use Text::CSV_XS;

our @EXPORT = qw(new_csv_in new_csv_out write_csv_line rename_to_backup_file_if_exists);

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

# rename_to_backup_file_if_exists $filename: renames a file into a backup
# file by appending '.bak' to its name. A previously existing backup file
# with the same name will be silently overwritten. If $filename doesn't exist,
# this function does nothing.
sub rename_to_backup_file_if_exists {
    my ($filename) = @_;
    rename $filename, "$filename.bak" if -e $filename;
}

1;
