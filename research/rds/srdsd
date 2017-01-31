#!/usr/bin/perl
#
# Simple RDS-TMC Decoder #20070420
#
# This Code is C.R.U.D.E.
# (Code Rushed and Ugly because of Unexpected DEadline)
#
# Copyright 2007 Andrea Barisani <andrea@inversepath.com>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

use strict;
use warnings;
use File::Temp qw(tempdir);
use Getopt::Long qw(:config no_ignore_case);

my %opts;
GetOptions('tmc' => \$opts{'t'}, 'Html' => \$opts{'H'}, 'PIsearch' => \$opts{'P'},
           'pi=s'=> \$opts{'p'}, 'db=s' => \$opts{'d'}, 'help' => \$opts{'h'});
if ($opts{'h'} or (!$ARGV[0])) { usage(); }

open(FILE, "<$ARGV[0]") or
    die "Could not open $ARGV[0]: $!\n";

if ($opts{'P'}) { pisearch(); exit 0; }

my $i = 1;
my @PSN = ();
my $index = 0;
my (@DP, @PN, @Event);
my ($findPI, $FH, $rds_msg, $table_path);
my (%point, %name, %road, %segment, %char, %di, %pty, %grp, %label);

my $PI  = hex2bin($opts{'p'}) || "0101001000000001"; # RADIO1
my $dir = "/tmp/rds-XXXXXX";

init_lookup_tables();

if ($opts{'d'}) {
    $table_path = $opts{'d'};
    read_tmc_table($table_path);
}

if ($opts{'H'}) {
    $dir = tempdir($dir);
    open(INDEX, ">$dir/index-rds.html");
    print INDEX "<html><body><pre>";
}

while(!eof(FILE)) {
    read(FILE,$findPI,16,0);
    $findPI =~ s/\*/0/g;
    $findPI =~ s/\+/1/g;

    if ($findPI == $PI) {
        $index++;
        read(FILE,$rds_msg,(104-16),0);
        $rds_msg =~ s/\*/0/g;
        $rds_msg =~ s/\+/1/g;
        parse_rds($rds_msg);
    }
    seek(FILE,$i,0); $i++;
}

if ($opts{'H'}) {
    print INDEX "</pre></body></html>";
    close INDEX;
}

sub parse_rds {

    return unless $_[0] =~ /^([01]{10})([01]{4})([01]{1})([01]{1})([01]{5})([01]{5})([01]{10})([01]{16})([01]{10})([01]{16})([01]{10})/;

    my ($sumA, $GRP, $VER, $TP, $PTY)       = ($1, $2, $3, $4, $5);
    my ($b2, $sumB, $b3, $sumC, $b4, $sumD) = ($6, $7, $8, $9, $10, $11);

    return if ($opts{'t'} && parse_grp($GRP . $VER) !~ /8A/);

    if ($opts{'H'}) {
        open($FH, ">$dir/rds-$index.html");
        print INDEX "<a href=\"rds-$index.html\">$index</a> - PI: " . bin2hex($PI) . " GRP: $GRP/$VER " . parse_grp($GRP . $VER) . "\n";
        print $FH "<html><body><pre>" if ($opts{'H'});
    } else {
        open($FH, ">-");
    }

    print $FH "Got RDS message (frame $index)\n",
              "\tProgramme Identification: $PI" . " (" . bin2hex($PI) . ")" . "\n",
              "\tGroup type code/version: $GRP/$VER" . " (" . parse_grp($GRP . $VER) . ")" . "\n",
              "\tTraffic Program: $TP\n",
              "\tProgramme Type: $PTY" . " (" . parse_pty($PTY) . ")" . "\n";

    if (parse_grp($GRP . $VER) =~ /0A/) {
        print $FH "\tDecoded 0A group:\n";
        parse_0A($b2, $b3, $b4);
    }

    if (parse_grp($GRP . $VER) =~ /8A/) {
        print $FH "\tDecoded 8A group:\n";
        parse_8A($b2, $b3, $b4);
    }

    if ((parse_grp($GRP . $VER) =~ /3A/) && ($b2 == "10000") && ($b4 == "1100110101000110")) { # AID = 52550
        print $FH "\tDecoded TMC Sys Info group (3A - AID 52550):\n";
        parse_3A_tmc($b3, $b4);
    }

    print $FH "\n\tRaw dump | Data             Checkword  Hex\n",
              "\tBlock 1: | $PI $sumA " . bin2hex($PI) . "\n",
              "\tBlock 2: | $GRP$VER$TP$PTY$b2 $sumB " . bin2hex($GRP . $VER . $TP . $PTY . $b2) . "\n",
              "\tBlock 3: | $b3 $sumC " . bin2hex($b3) . "\n",
              "\tBlock 4: | $b4 $sumD " . bin2hex($b4) . "\n\n";

    print $FH "</pre></html></body>" if ($opts{'H'});
    close $FH;
}

sub parse_0A {
    my ($TA, $MS, $DI, $C1, $C0) = shift =~ /([01]{1})([01]{1})([01]{1})([01]{1})([01]{1})/;
    my ($AF1, $AF2)              = shift =~ /([01]{8})([01]{8})/;
    my ($B1A, $B1B, $B2A, $B2B)  = shift =~ /([01]{4})([01]{4})([01]{4})([01]{4})/;

    my $char1 = parse_char($B1A, $B1B);
    my $char2 = parse_char($B2A, $B2B);

    print $FH "\t\tTraffic Announcement: $TA\n",
              "\t\tMusic Speech switch: $TA\n",
              "\t\tDecoder Identification control: $DI$C1$C0 (" . parse_di($DI, $C1, $C0) . ")\n",
              "\t\tAlternative Frequencies: $AF1, $AF2 (" . parse_AF($AF1) . ", " . parse_AF($AF2) . ")\n",
              "\t\tProgramme Service name: $B1A$B1B$B2A$B2B (" . $char1 . $char2 . ")\n";

    if (($C1 . $C0) eq "11") { $PSN[3] = "$char1$char2"; }
    if (($C1 . $C0) eq "10") { $PSN[2] = "$char1$char2"; }
    if (($C1 . $C0) eq "01") { $PSN[1] = "$char1$char2"; }
    if (($C1 . $C0) eq "00") { $PSN[0] = "$char1$char2"; }

    print $FH "\t\tCollected PSN: ";
    foreach my $char (@PSN) {
        if ($char) { print $FH $char; }
    }
    print $FH "\n";
}

sub parse_8A {
    my ($T, $F, $DP)              = $_[0] =~ /([01]{1})([01]{1})([01]{3})/;
    my ($D, $PN, $Extent, $Event) = $_[1] =~ /([01]{1})([01]{1})([01]{3})([01]{11})/;
    my ($Location)                = $_[2] =~ /([01]{16})/;

    if ($_[0] == "00000") {
        my ($v) = $_[1] =~ /([01]{3})[01]{13}/;

        print $FH "\t\t<Encrypted Service>\n";
        print $FH "\t\tVariant " . bin2dec($v) . "\n";

        if ($v == "000") {
            my ($test, $SID, $ENCID) = $_[1] =~ /000([01]{2})([01]{6})([01]{5})/;
            my ($LTNBE)              = $_[2] =~ /([01]{6})[01]{10}/;

            print $FH "\t\tTest bits: $test\n",
                      "\t\tService Identifier: $SID (" . bin2dec($SID) . ")\n",
                      "\t\tEncryption Identifier: $ENCID (" . bin2dec($ENCID) . ")\n",
                      "\t\tLocation Table Number: $LTNBE (" . bin2dec($LTNBE) . ")\n";
        }
        print $FH "\n";
        return;
    }

    print $FH "\t\tBit X4: $T (" . parse_T($T) . ")\n",
              "\t\tBit X3: $F (" . parse_F($F);

    if (parse_F($F) =~ /Multi/) {
        if ($T == 0 && $F == 0 && $D == 1) {
            print $FH " - first group)\n",
                      "\t\tContinuity Index: $DP\n",
                      "\t\tDirection: $PN (" . parse_PN($PN) . ")\n",
                      "\t\tExtent: $Extent (" . bin2dec($Extent) . ")\n",
                      "\t\tEvent: $Event (" . bin2dec($Event) . " - " . parse_Event($Event) . ")\n",
                      "\t\tLocation: $Location (" . bin2dec($Location) . ")\n",
                      "\t\tDecoded Location:\n";
            parse_location(bin2dec($Location));
        } elsif ($T == 0 && $F == 0 && $D == 0) {

            my ($GSI, $multi1) = $_[1]               =~ /[01]{2}([01]{2})([01]{12})/;
            my ($multi2)       = $_[2]               =~ /([01]{16})/;

            print $FH " - subsequent group)\n",
                      "\t\tContinuity Index: $DP\n",
                      "\t\tSecond Group indicator: $PN\n",
                      "\t\tGroup Sequence Identifier: $GSI (" . bin2dec($GSI) . ")\n",
	              "\t\tFree Block: $multi1$multi2\n";

            return if $PN == "0"; # we don't have concatenation logic yet

            my $multi = ($multi1 . $multi2);
            my ($lcount, $seek, $retl, $retv) = (1, 1, 1, 1);
            my ($label, $value, $label_d, $value_d, $label_msg);

            while ($seek) {
                $lcount++;

                $retl = $multi =~ s/^([01]{4})//;
                $label     = $1;
                $label_d   = bin2dec($1);
                $label_msg = $label{$label_d}{'msg'} || "unknown";

                if ($retl) {
                    $retv = $multi =~ s/^([01]{$label{bin2dec($label)}{'size'}})//;
                    $value   = $1;
                    $value_d = bin2dec($1);
                }

                if ($label == 0 && $value == 0) {
                    $seek = 0; next;
                }

                if (!$retl) {
                    print $FH "\t\t\tLabel $lcount: partial?\n";
                    $seek = 0; next;
                } else {
                    print $FH "\t\t\tLabel $lcount: $label (" . $label_d . " - " . $label_msg . ")\n",
                }

                if (!$retv) {
                    print $FH "\t\t\tValue $lcount: partial?\n";
                    $seek = 0; next;
                } else {
                    print $FH "\t\t\tValue $lcount: $value (" . $value_d . ")\n";
                }
            }
        }
    } else {
        print $FH ")\n";
    }

    if (parse_F($F) =~ /Single/) {
        print $FH "\t\tDuration and Persistence: $DP (" . parse_DP($DP) . ")\n",
                  "\t\tDiversion advice: $D\n",
                  "\t\tDirection: $PN (" . parse_PN($PN) . ")\n",
                  "\t\tExtent: $Extent (" . bin2dec($Extent) . ")\n",
                  "\t\tEvent: $Event (" . bin2dec($Event) . " - " . parse_Event($Event) . ")\n",
                  "\t\tLocation: $Location (" . bin2dec($Location) . ")\n",
                  "\t\tDecoded Location:\n";
        parse_location(bin2dec($Location));
    }
}

sub parse_3A_tmc {
    my ($Y15, $Y14) = $_[0] =~ /([01]{1})([01]{1})[01]{14}/;
    my ($AID)       = $_[1] =~ /([01]{16})/;

    if (($Y15 . $Y14) == "00") {
        my ($LTN, $AFI, $M, $I, $N, $R, $U) = $_[0] =~ /[01]{4}([01]{6})([01]{1})([01]{1})([01]{1})([01]{1})([01]{1})([01]{1})/;

        print $FH "\t\tLocation Table Number: $LTN (" . bin2dec($LTN) . ")\n",
                  "\t\tAlternative Frequency bit: $AFI\n",
                  "\t\tMode of Transmission: $M\n",
                  "\t\tInternational Scope: $I\n",
                  "\t\tNational Scope: $N\n",
                  "\t\tRegional Scope: $R\n",
                  "\t\tUrban Scope: $U\n",
    }

    if (($Y15 . $Y14) == "01") {
        my ($G, $SID) = $_[0] =~ /[01]{2}([01]{2})([01]{6})[01]{6}/;

        print $FH "\t\tGap index: $G (" . bin2dec($G) . ")\n",
                  "\t\tService Identifier: $SID (" . bin2dec($SID) . ")\n",
    }

    print $FH "\t\tAID: $AID (" . bin2dec($AID) . ")\n";
}


sub parse_AF {
    return (87.5 + bin2dec($_[0])/10);
}

sub parse_char {
    my $char = $char{"$_[0]"}{"$_[1]"};
    return (defined($char))?$char:'?';
}

sub parse_T {
    return "User message"       if ($_[0] == "0");
    return "Tuning Information" if ($_[0] == "1");
}

sub parse_F {
    return "Multi-group message"  if ($_[0] == "0");
    return "Single-group message" if ($_[0] == "1");
}

sub parse_DP {
    return $DP[bin2dec($_[0])] || "unknown";
}

sub parse_PN {
    return $PN[$_[0]] || "unknown";
}

sub parse_Event {
    return $Event[bin2dec($_[0])] || "unknown";
}

sub parse_di {
    return $di{$_[0] . $_[1] . $_[2]} || "unknown";
}

sub parse_pty {
    return $pty{"$_[0]"} || "unknown";
}

sub parse_grp {
    return $grp{"$_[0]"} || "unknown";
}

sub parse_location {

    if (!$opts{'d'}) {
        print $FH "\t\t\t<no location table specified>\n";
        return;
    }

    my $code = $_[0];

    if ($point{$code}) {
        my $x       = $point{$code}{'XCOORD'};
        my $y       = $point{$code}{'YCOORD'};
        my $nid     = $point{$code}{'N1ID'};
        my $road    = $point{$code}{'ROA_LCD'};
        my $segment = $point{$code}{'SEG_LCD'};

        print $FH "\t\t\tLocation code type: POINT\n";
        print $FH "\t\t\tName ID: $nid ($name{$nid})\n"                         if $nid;
        print $FH "\t\t\tRoad code: $road ($road{$road}{'NAME'})\n"             if $road;
        print $FH "\t\t\tSegment code: $segment ($segment{$segment}{'NAME'})\n" if $segment;
        if ($y && $x) {
            print $FH "\t\t\tGPS: $y N $x E\n";
            if ($opts{'H'}) {
                print $FH "\t\t\t<a href=\"http://maps.google.com/maps?ll=" . $y . "," . $x . "&spn=0.3,0.3&q=" . $y . "," . $x . "\">Map Link</a>\n";
            } else {
            print $FH "\t\t\tLink: http://maps.google.com/maps?ll=" . $y . "," . $x . "&spn=0.3,0.3&q=" . $y . "," . $x . "\n";
            }
        }
    } elsif ($road{$code}) {
        my $n1id    = $road{$code}{'N1ID'};
        my $n2id    = $road{$code}{'N2ID'};
        my $name    = $road{$code}{'NAME'};
        my $road    = $point{$code}{'ROA_LCD'};
        my $segment = $point{$code}{'SEG_LCD'};

        print $FH "\t\t\tLocation code type: ROAD\n";
        print $FH "\t\t\tName IDs: $n1id - $n2id ($name)\n"                      if ($n1id && $n2id);
        print $FH "\t\t\tRoad code: $road ($road{$road}{'NAME'})\n"              if $road;
        print $FH "\t\t\tSegment code: $segment ($segment{$segment}{'NAME'})\n"  if $segment;
    } elsif ($segment{$code}) {
        my $n1id    = $segment{$code}{'N1ID'};
        my $n2id    = $segment{$code}{'N2ID'};
        my $name    = $segment{$code}{'NAME'};
        my $road    = $point{$code}{'ROA_LCD'};
        my $segment = $point{$code}{'SEG_LCD'};
        my $point   = $point{$code}{'POL_LCD'};

        print $FH "\t\t\tLocation code type: SEGMENT\n";
        print $FH "\t\t\tName IDs: $n1id - $n2id ($name)\n"                     if ($n1id && $n2id);
        print $FH "\t\t\tRoad code: $road ($road{$road}{'NAME'})\n"             if $road;
        print $FH "\t\t\tSegment code: $segment ($segment{$segment}{'NAME'})\n" if $segment;
    } else {
        print $FH "\t\t\tUnknown Location\n";
    }
}

sub read_tmc_table {

    my $count = 0;
    my $table_path = $_[0];
    print "Reading TMC Location Table at $table_path:\n";

    open(POINTS,   "<$table_path/POINTS.dat")   or die "Could not open $table_path/POINTS.dat: $!\n";
    open(NAMES,    "<$table_path/NAMES.dat")    or die "Could not open $table_path/NAMES.dat: $!\n";
    open(ROADS,    "<$table_path/ROADS.dat")    or die "Could not open $table_path/ROADS.dat: $!\n";
    open(SEGMENTS, "<$table_path/SEGMENTS.dat") or die "Could not open $table_path/SEGMENTS.dat: $!\n";

    print "\t parsing NAMES: ";
    while (<NAMES>) {
        next unless /^[0-9]/;

        my @line = split (/;/, $_);
        my ($CID, $LID, $NID, $NAME, $NCOMMENT) = @line;

        $name{$NID} = $NAME;
        $count++;
    }
    print "$count entries\n"; $count = 0;

    print "\t parsing ROADS: ";
    while (<ROADS>) {
        next unless /^[0-9]/;

        my @line = split (/;/, $_);
        my ($CID, $TABC, $LCD, $CLASS, $TCD, $STCD, $ROADNUMBER, $RNID, $N1ID, $N2ID, $POL_LCD, $PES_LEV) = @line;

        $road{$LCD}{'ROADNUMBER'} = $ROADNUMBER;
        $road{$LCD}{'N1ID'}       = $N1ID;
        $road{$LCD}{'N2ID'}       = $N2ID;
        $road{$LCD}{'POL_LCD'}    = $POL_LCD;
        $road{$LCD}{'NAME'}       = $name{$N1ID} . "-" . $name{$N2ID};
        $count++;
    }
    print "$count entries\n"; $count = 0;

    print "\t parsing SEGMENTS: ";
    while (<SEGMENTS>) {
        next unless /^[0-9]/;

        my @line = split (/;/, $_);
        my ($CID, $TABC, $LCD, $CLASS, $TCD, $STCD, $ROADNUMBER, $RNID, $N1ID, $N2ID, $ROA_LCD, $SEG_LCD, $POL_LCD) = @line;

        $segment{$LCD}{'ROADNUMBER'} = $ROADNUMBER;
        $segment{$LCD}{'N1ID'}       = $N1ID;
        $segment{$LCD}{'N2ID'}       = $N1ID;
        $segment{$LCD}{'ROA_LCD'}    = $ROA_LCD;
        $segment{$LCD}{'SEG_LCD'}    = $SEG_LCD;
        $segment{$LCD}{'POL_LCD'}    = $POL_LCD;
        $segment{$LCD}{'NAME'}       = $name{$N1ID} . "-" . $name{$N2ID};
        $count++;
    }
    print "$count entries\n"; $count = 0;

    print "\t parsing POINTS: ";
    while (<POINTS>) {
        next unless /^[0-9]/;

        my @line = split (/;/, $_);
        my ($CID, $TABCD, $LCD, $CLASS, $TCD, $STCD, $JUNCTIONMUMBER, $RNID, $N1ID, $N2ID, $POL_LCD, $OTH_LCD, $SEG_LCD, $ROA_LCD, $INPOS, $INNEG, $OUTPOS, $OUTNEG, $PRESENTPOS, $PRESENTNEG, $DIVERSIONPOS, $DIVERSIONNEG, $XCOORD, $YCOORD, $INTERRUPTSROAD, $URNAB) = @line;

        my $x = ($XCOORD/100000);
        my $y = ($YCOORD/100000);

        $point{$LCD}{'XCOORD'}   = $x;
        $point{$LCD}{'YCOORD'}   = $y;
        $point{$LCD}{'RNID'}     = $RNID;
        $point{$LCD}{'N1ID'}     = $N1ID;
        $point{$LCD}{'ROA_LCD'}  = $ROA_LCD;
        $point{$LCD}{'SEG_LCD'}  = $SEG_LCD;
        $count++;
    }
    print "$count entries\n"; $count = 0;
    print "done.\n\n";
}

sub bin2hex {
    return unpack("H*", pack("B*", $_[0]));
}

sub hex2bin {
    return unpack("B*", pack("H*", $_[0]));
}

sub bin2dec {
    return unpack("N", pack("B32", substr("0" x 32 . $_[0], -32)));
}

sub dec2bin {
    my $str = unpack("B32", pack("N", $_[0]));
    $str =~ s/^0+(?=\d)//;
    return $str;
}

sub pisearch {

    my $string;
    my %hash;
    my $i = 0;

    open(FILE, "<$ARGV[0]");

    while(!eof(FILE)) {
        read(FILE,$string,16,0);
        $hash{$string}++;
        seek(FILE,$i,0); $i++;
    }

    foreach my $key (sort { $hash{$a} <=> $hash{$b} } keys %hash) {
        print "$key: $hash{$key} (" . bin2hex($key) .")\n";
    }
}

sub init_lookup_tables {

    # characters table (yes we like binary)

    $char{'0010'}{'0000'} = " "; $char{'0010'}{'0111'} = "'"; $char{'0010'}{'1100'} = "`";
    $char{'0010'}{'1101'} = "-"; $char{'0010'}{'1110'} = "."; $char{'0010'}{'1111'} = "/";

    $char{'0011'}{'0000'} = "0"; $char{'0011'}{'0001'} = "1"; $char{'0011'}{'0010'} = "2";
    $char{'0011'}{'0011'} = "3"; $char{'0011'}{'0100'} = "4"; $char{'0011'}{'0101'} = "5";
    $char{'0011'}{'0110'} = "6"; $char{'0011'}{'0111'} = "7"; $char{'0011'}{'1000'} = "8";
    $char{'0011'}{'1001'} = "8"; $char{'0011'}{'1010'} = ":"; $char{'0011'}{'1011'} = ";";
    $char{'0011'}{'1100'} = "<"; $char{'0011'}{'1101'} = "="; $char{'0011'}{'1110'} = ">";
    $char{'0011'}{'1111'} = "?";

    $char{'0100'}{'0000'} = "@"; $char{'0100'}{'0001'} = "A"; $char{'0100'}{'0010'} = "B";
    $char{'0100'}{'0011'} = "C"; $char{'0100'}{'0100'} = "D"; $char{'0100'}{'0101'} = "E";
    $char{'0100'}{'0110'} = "F"; $char{'0100'}{'0111'} = "G"; $char{'0100'}{'1000'} = "H";
    $char{'0100'}{'1001'} = "I"; $char{'0100'}{'1010'} = "J"; $char{'0100'}{'1011'} = "K";
    $char{'0100'}{'1100'} = "L"; $char{'0100'}{'1101'} = "M"; $char{'0100'}{'1110'} = "N";
    $char{'0100'}{'1111'} = "O";

    $char{'0101'}{'0000'} = "P"; $char{'0101'}{'0001'} = "Q"; $char{'0101'}{'0010'} = "R";
    $char{'0101'}{'0011'} = "S"; $char{'0101'}{'0100'} = "T"; $char{'0101'}{'0101'} = "U";
    $char{'0101'}{'0110'} = "V"; $char{'0101'}{'0111'} = "W"; $char{'0101'}{'1000'} = "X";
    $char{'0101'}{'1001'} = "Y"; $char{'0101'}{'1010'} = "Z"; $char{'0101'}{'1011'} = "[";

    # Duration code

    $DP[0] = "no explicit duration given"; $DP[1] = "15 minutes"; $DP[2] = "30 minutes"; $DP[3] = "1 hour";
    $DP[4] = "2 hours";                    $DP[5] = "3 hours";    $DP[6] = "4 hours";    $DP[7] = "all day";

    # Direction

    $PN[0] = "+";
    $PN[1] = "-";

    # Event codes

    $Event[1]    = "traffic problem";
    $Event[2]    = "queuing traffic (with average speeds Q). Danger of stationary traffic";
    $Event[11]   = "overheight warning system triggered";
    $Event[12]   = "(Q) accident(s), traffic being directed around accident area";
    $Event[16]   = "closed, rescue and recovery work in progress";
    $Event[20]   = "service area overcrowded, drive to another service area";
    $Event[22]   = "service area, fuel station closed";
    $Event[23]   = "service area, restaurant closed";
    $Event[24]   = "bridge closed";
    $Event[25]   = "tunnel closed";
    $Event[26]   = "bridge blocked";
    $Event[27]   = "tunnel blocked";
    $Event[28]   = "road closed intermittently";
    $Event[36]   = "fuel station reopened";
    $Event[37]   = "restaurant reopened";
    $Event[40]   = "smog alert ended";
    $Event[41]   = "(Q) overtaking lane(s) closed";
    $Event[42]   = "(Q) overtaking lane(s) blocked";
    $Event[51]   = "roadworks, (Q) overtaking lane(s) closed";
    $Event[52]   = "(Q sets of) roadworks on the hard shoulder";
    $Event[53]   = "(Q sets of) roadworks in the emergency lane";
    $Event[55]   = "traffic problem expected";
    $Event[56]   = "traffic congestion expected";
    $Event[57]   = "normal traffic expected";
    $Event[61]   = "(Q) object(s) on roadway {something that does not neccessarily block the ro";
    $Event[62]   = "(Q) burst pipe(s)";
    $Event[63]   = "(Q) object(s) on the road. Danger";
    $Event[64]   = "burst pipe. Danger";
    $Event[70]   = "traffic congestion, average speed of km/h";
    $Event[71]   = "traffic congestion, average speed of km/h";
    $Event[72]   = "traffic congestion, average speed of km/h";
    $Event[73]   = "traffic congestion, average speed of km/h";
    $Event[74]   = "traffic congestion, average speed of km/h";
    $Event[75]   = "traffic congestion, average speed of km/h";
    $Event[76]   = "traffic congestion, average speed of km/h";
    $Event[91]   = "delays (Q) for cars";
    $Event[101]  = "stationary traffic";
    $Event[102]  = "stationary traffic for km";
    $Event[103]  = "stationary traffic for km";
    $Event[104]  = "stationary traffic for km";
    $Event[105]  = "stationary traffic for km";
    $Event[106]  = "stationary traffic for km";
    $Event[107]  = "stationary traffic expected";
    $Event[108]  = "queuing traffic (with average speeds Q)";
    $Event[109]  = "queuing traffic for km (with average speeds Q)";
    $Event[110]  = "queuing traffic for km (with average speeds Q)";
    $Event[111]  = "queuing traffic for km (with average speeds Q)";
    $Event[112]  = "queuing traffic for km (with average speeds Q)";
    $Event[113]  = "queuing traffic for km (with average speeds Q)";
    $Event[114]  = "queuing traffic expected";
    $Event[115]  = "slow traffic (with average speeds Q)";
    $Event[116]  = "slow traffic for km (with average speeds Q)";
    $Event[117]  = "slow traffic for km (with average speeds Q)";
    $Event[118]  = "slow traffic for km (with average speeds Q)";
    $Event[119]  = "slow traffic for km (with average speeds Q)";
    $Event[120]  = "slow traffic for km (with average speeds Q)";
    $Event[121]  = "slow traffic expected";
    $Event[122]  = "heavy traffic (with average speeds Q)";
    $Event[123]  = "heavy traffic expected";
    $Event[124]  = "traffic flowing freely (with average speeds Q)";
    $Event[125]  = "traffic building up (with average speeds Q)";
    $Event[126]  = "no problems to report";
    $Event[127]  = "traffic congestion cleared";
    $Event[128]  = "message cancelled";
    $Event[129]  = "stationary traffic for km";
    $Event[130]  = "danger of stationary traffic";
    $Event[131]  = "queuing traffic for km (with average speeds Q)";
    $Event[132]  = "danger of queuing traffic (with average speeds Q)";
    $Event[133]  = "long queues (with average speeds Q)";
    $Event[134]  = "slow traffic for km (with average speeds Q)";
    $Event[135]  = "traffic easing";
    $Event[136]  = "traffic congestion (with average speeds Q)";
    $Event[137]  = "traffic lighter than normal (with average speeds Q)";
    $Event[138]  = "queuing traffic (with average speeds Q). Approach with care";
    $Event[139]  = "queuing traffic around a bend in the road";
    $Event[140]  = "queuing traffic over the crest of a hill";
    $Event[141]  = "all accidents cleared, no problems to report";
    $Event[142]  = "traffic heavier than normal (with average speeds Q)";
    $Event[143]  = "traffic very much heavier than normal (with average speeds Q)";
    $Event[200]  = "multi vehicle pile up. Delays (Q)";
    $Event[201]  = "(Q) accident(s)";
    $Event[202]  = "(Q) serious accident(s)";
    $Event[203]  = "multi-vehicle accident (involving Q vehicles)";
    $Event[204]  = "accident involving (a/Q) heavy lorr(y/ies)";
    $Event[205]  = "(Q) accident(s) involving hazardous materials";
    $Event[206]  = "(Q) fuel spillage accident(s)";
    $Event[207]  = "(Q) chemical spillage accident(s)";
    $Event[208]  = "vehicles slowing to look at (Q) accident(s)";
    $Event[209]  = "(Q) accident(s) in the opposing lanes";
    $Event[210]  = "(Q) shed load(s)";
    $Event[211]  = "(Q) broken down vehicle(s)";
    $Event[212]  = "(Q) broken down heavy lorr(y/ies)";
    $Event[213]  = "(Q) vehicle fire(s)";
    $Event[214]  = "(Q) incident(s)";
    $Event[215]  = "(Q) accident(s). Stationary traffic";
    $Event[216]  = "(Q) accident(s). Stationary traffic for km";
    $Event[217]  = "(Q) accident(s). Stationary traffic for km";
    $Event[218]  = "(Q) accident(s). Stationary traffic for km";
    $Event[219]  = "(Q) accident(s). Stationary traffic for km";
    $Event[220]  = "(Q) accident(s). Stationary traffic for km";
    $Event[221]  = "(Q) accident(s). Danger of stationary traffic";
    $Event[222]  = "(Q) accident(s). Queuing traffic";
    $Event[223]  = "(Q) accident(s). Queuing traffic for km";
    $Event[224]  = "(Q) accident(s). Queuing traffic for km";
    $Event[225]  = "(Q) accident(s). Queuing traffic for km";
    $Event[226]  = "(Q) accident(s). Queuing traffic for km";
    $Event[227]  = "(Q) accident(s). Queuing traffic for km";
    $Event[228]  = "(Q) accident(s). Danger of queuing traffic";
    $Event[229]  = "(Q) accident(s). Slow traffic";
    $Event[230]  = "(Q) accident(s). Slow traffic for km";
    $Event[231]  = "(Q) accident(s). Slow traffic for km";
    $Event[232]  = "(Q) accident(s). Slow traffic for km";
    $Event[233]  = "(Q) accident(s). Slow traffic for km";
    $Event[234]  = "(Q) accident(s). Slow traffic for km";
    $Event[235]  = "(Q) accident(s). Slow traffic expected";
    $Event[236]  = "(Q) accident(s). Heavy traffic";
    $Event[237]  = "(Q) accident(s). Heavy traffic expected";
    $Event[238]  = "(Q) accident(s). Traffic flowing freely";
    $Event[239]  = "(Q) accident(s). Traffic building up";
    $Event[240]  = "road closed due to (Q) accident(s)";
    $Event[241]  = "(Q) accident(s). Right lane blocked";
    $Event[242]  = "(Q) accident(s). Centre lane blocked";
    $Event[243]  = "(Q) accident(s). Left lane blocked";
    $Event[244]  = "(Q) accident(s). Hard shoulder blocked";
    $Event[245]  = "(Q) accident(s). Two lanes blocked";
    $Event[246]  = "(Q) accident(s). Three lanes blocked";
    $Event[247]  = "accident. Delays (Q)";
    $Event[248]  = "accident. Delays (Q) expected";
    $Event[249]  = "accident. Long delays (Q)";
    $Event[250]  = "vehicles slowing to look at (Q) accident(s). Stationary traffic";
    $Event[251]  = "vehicles slowing to look at (Q) accident(s). Stationary traffic for km";
    $Event[252]  = "vehicles slowing to look at (Q) accident(s). Stationary traffic for km";
    $Event[253]  = "vehicles slowing to look at (Q) accident(s). Stationary traffic for km";
    $Event[254]  = "vehicles slowing to look at (Q) accident(s). Stationary traffic for km";
    $Event[255]  = "vehicles slowing to look at (Q) accident(s). Stationary traffic for km";
    $Event[256]  = "vehicles slowing to look at (Q) accident(s). Danger of stationary traffic";
    $Event[257]  = "vehicles slowing to look at (Q) accident(s). Queuing traffic";
    $Event[258]  = "vehicles slowing to look at (Q) accident(s). Queuing traffic for km";
    $Event[259]  = "vehicles slowing to look at (Q) accident(s). Queuing traffic for km";
    $Event[260]  = "vehicles slowing to look at (Q) accident(s). Queuing traffic for km";
    $Event[261]  = "vehicles slowing to look at (Q) accident(s). Queuing traffic for km";
    $Event[262]  = "vehicles slowing to look at (Q) accident(s). Queuing traffic for km";
    $Event[263]  = "vehicles slowing to look at (Q) accident(s). Danger of queuing traffic";
    $Event[264]  = "vehicles slowing to look at (Q) accident(s). Slow traffic";
    $Event[265]  = "vehicles slowing to look at (Q) accident(s). Slow traffic for km";
    $Event[266]  = "vehicles slowing to look at (Q) accident(s). Slow traffic for km";
    $Event[267]  = "vehicles slowing to look at (Q) accident(s). Slow traffic for km";
    $Event[268]  = "vehicles slowing to look at (Q) accident(s). Slow traffic for km";
    $Event[269]  = "vehicles slowing to look at (Q) accident(s). Slow traffic for km";
    $Event[270]  = "vehicles slowing to look at (Q) accident(s). Slow traffic expected";
    $Event[271]  = "vehicles slowing to look at (Q) accident(s). Heavy traffic";
    $Event[272]  = "vehicles slowing to look at (Q) accident(s). Heavy traffic expected";
    $Event[274]  = "vehicles slowing to look at (Q) accident(s). Traffic building up";
    $Event[275]  = "vehicles slowing to look at accident. Delays (Q)";
    $Event[276]  = "vehicles slowing to look at accident. Delays (Q) expected";
    $Event[277]  = "vehicles slowing to look at accident. Long delays (Q)";
    $Event[278]  = "(Q) shed load(s). Stationary traffic";
    $Event[279]  = "(Q) shed load(s). Stationary traffic for km";
    $Event[280]  = "(Q) shed load(s). Stationary traffic for km";
    $Event[281]  = "(Q) shed load(s). Stationary traffic for km";
    $Event[282]  = "(Q) shed load(s). Stationary traffic for km";
    $Event[283]  = "(Q) shed load(s). Stationary traffic for km";
    $Event[284]  = "(Q) shed load(s). Danger of stationary traffic";
    $Event[285]  = "(Q) shed load(s). Queuing traffic";
    $Event[286]  = "(Q) shed load(s). Queuing traffic for km";
    $Event[287]  = "(Q) shed load(s). Queuing traffic for km";
    $Event[288]  = "(Q) shed load(s). Queuing traffic for km";
    $Event[289]  = "(Q) shed load(s). Queuing traffic for km";
    $Event[290]  = "(Q) shed load(s). Queuing traffic for km";
    $Event[291]  = "(Q) shed load(s). Danger of queuing traffic";
    $Event[292]  = "(Q) shed load(s). Slow traffic";
    $Event[293]  = "(Q) shed load(s). Slow traffic for km";
    $Event[294]  = "(Q) shed load(s). Slow traffic for km";
    $Event[295]  = "(Q) shed load(s). Slow traffic for km";
    $Event[296]  = "(Q) shed load(s). Slow traffic for km";
    $Event[297]  = "(Q) shed load(s). Slow traffic for km";
    $Event[298]  = "(Q) shed load(s). Slow traffic expected";
    $Event[299]  = "(Q) shed load(s). Heavy traffic";
    $Event[300]  = "(Q) shed load(s). Heavy traffic expected";
    $Event[301]  = "(Q) shed load(s). Traffic flowing freely";
    $Event[302]  = "(Q) shed load(s). Traffic building up";
    $Event[303]  = "blocked by (Q) shed load(s)";
    $Event[304]  = "(Q) shed load(s). Right lane blocked";
    $Event[305]  = "(Q) shed load(s). Centre lane blocked";
    $Event[306]  = "(Q) shed load(s). Left lane blocked";
    $Event[307]  = "(Q) shed load(s). Hard shoulder blocked";
    $Event[308]  = "(Q) shed load(s). Two lanes blocked";
    $Event[309]  = "(Q) shed load(s). Three lanes blocked";
    $Event[310]  = "shed load. Delays (Q)";
    $Event[311]  = "shed load. Delays (Q) expected";
    $Event[312]  = "shed load. Long delays (Q)";
    $Event[313]  = "(Q) broken down vehicle(s). Stationary traffic";
    $Event[314]  = "(Q) broken down vehicle(s). Danger of stationary traffic";
    $Event[315]  = "(Q) broken down vehicle(s). Queuing traffic";
    $Event[316]  = "(Q) broken down vehicle(s). Danger of queuing traffic";
    $Event[317]  = "(Q) broken down vehicle(s). Slow traffic";
    $Event[318]  = "(Q) broken down vehicle(s). Slow traffic expected";
    $Event[319]  = "(Q) broken down vehicle(s). Heavy traffic";
    $Event[320]  = "(Q) broken down vehicle(s). Heavy traffic expected";
    $Event[321]  = "(Q) broken down vehicle(s). Traffic flowing freely";
    $Event[322]  = "(Q) broken down vehicle(s).Traffic building up";
    $Event[323]  = "blocked by (Q) broken down vehicle(s).";
    $Event[324]  = "(Q) broken down vehicle(s). Right lane blocked";
    $Event[325]  = "(Q) broken down vehicle(s). Centre lane blocked";
    $Event[326]  = "(Q) broken down vehicle(s). Left lane blocked";
    $Event[327]  = "(Q) broken down vehicle(s). Hard shoulder blocked";
    $Event[328]  = "(Q) broken down vehicle(s). Two lanes blocked";
    $Event[329]  = "(Q) broken down vehicle(s). Three lanes blocked";
    $Event[330]  = "broken down vehicle. Delays (Q)";
    $Event[331]  = "broken down vehicle. Delays (Q) expected";
    $Event[332]  = "broken down vehicle. Long delays (Q)";
    $Event[333]  = "accident cleared";
    $Event[334]  = "message cancelled";
    $Event[335]  = "accident involving (a/Q) bus(es)";
    $Event[336]  = "(Q) oil spillage accident(s)";
    $Event[337]  = "(Q) overturned vehicle(s)";
    $Event[338]  = "(Q) overturned heavy lorr(y/ies)";
    $Event[339]  = "(Q) jackknifed trailer(s)";
    $Event[340]  = "(Q) jackknifed caravan(s)";
    $Event[341]  = "(Q) jackknifed articulated lorr(y/ies)";
    $Event[342]  = "(Q) vehicle(s) spun around";
    $Event[343]  = "(Q) earlier accident(s)";
    $Event[344]  = "accident investigation work";
    $Event[345]  = "(Q) secondary accident(s)";
    $Event[346]  = "(Q) broken down bus(es)";
    $Event[347]  = "(Q) overheight vehicle(s)";
    $Event[348]  = "(Q) accident(s). Stationary traffic for km";
    $Event[349]  = "(Q) accident(s). Queuing traffic for km";
    $Event[350]  = "(Q) accident(s). Slow traffic for km";
    $Event[351]  = "(Q) accident(s) in roadworks area";
    $Event[352]  = "vehicles slowing to look at (Q) accident(s). Stationary traffic for km";
    $Event[353]  = "vehicles slowing to look at (Q) accident(s). Queuing traffic for km";
    $Event[354]  = "vehicles slowing to look at (Q) accident(s). Slow traffic for km";
    $Event[355]  = "vehicles slowing to look at (Q) accident(s). Danger";
    $Event[356]  = "(Q) shed load(s). Stationary traffic for km";
    $Event[357]  = "(Q) shed load(s). Queuing traffic for km";
    $Event[358]  = "(Q) shed load(s). Slow traffic for km";
    $Event[359]  = "(Q) shed load(s). Danger";
    $Event[360]  = "(Q) overturned vehicle(s). Stationary traffic";
    $Event[361]  = "(Q) overturned vehicle(s). Danger of stationary traffic";
    $Event[362]  = "(Q) overturned vehicle(s). Queuing traffic";
    $Event[363]  = "(Q) overturned vehicle(s). Danger of queuing traffic";
    $Event[364]  = "(Q) overturned vehicle(s). Slow traffic";
    $Event[365]  = "(Q) overturned vehicle(s). Slow traffic expected";
    $Event[366]  = "(Q) overturned vehicle(s). Heavy traffic";
    $Event[367]  = "(Q) overturned vehicle(s). Heavy traffic expected";
    $Event[368]  = "(Q) overturned vehicle(s). Traffic building up";
    $Event[369]  = "blocked by (Q) overturned vehicle(s)";
    $Event[370]  = "(Q) overturned vehicle(s). Right lane blocked";
    $Event[371]  = "(Q) overturned vehicle(s). Centre lane blocked";
    $Event[372]  = "(Q) overturned vehicle(s). Left lane blocked";
    $Event[373]  = "(Q) overturned vehicle(s). Two lanes blocked";
    $Event[374]  = "(Q) overturned vehicle(s). Three lanes blocked";
    $Event[375]  = "overturned vehicle. Delays (Q)";
    $Event[376]  = "overturned vehicle. Delays (Q) expected";
    $Event[377]  = "overturned vehicle. Long delays (Q)";
    $Event[378]  = "(Q) overturned vehicle(s). Danger";
    $Event[379]  = "Stationary traffic due to (Q) earlier accident(s)";
    $Event[380]  = "Danger of stationary traffic due to (Q) earlier accident(s)";
    $Event[381]  = "Queuing traffic due to (Q) earlier accident(s)";
    $Event[382]  = "Danger of queuing traffic due to (Q) earlier accident(s)";
    $Event[383]  = "Slow traffic due to (Q) earlier accident(s)";
    $Event[385]  = "Heavy traffic due to (Q) earlier accident(s)";
    $Event[387]  = "Traffic building up due to (Q) earlier accident(s)";
    $Event[388]  = "Delays (Q) due to earlier accident";
    $Event[390]  = "Long delays (Q) due to earlier accident";
    $Event[391]  = "accident investigation work. Danger";
    $Event[392]  = "(Q) secondary accident(s). Danger";
    $Event[393]  = "(Q) broken down vehicle(s). Danger";
    $Event[394]  = "(Q) broken down heavy lorr(y/ies). Danger";
    $Event[395]  = "road cleared";
    $Event[396]  = "incident cleared";
    $Event[397]  = "rescue and recovery work in progress";
    $Event[399]  = "message cancelled";
    $Event[401]  = "closed";
    $Event[402]  = "blocked";
    $Event[403]  = "closed for heavy vehicles (over Q)";
    $Event[404]  = "no through traffic for heavy lorries (over Q)";
    $Event[405]  = "no through traffic";
    $Event[406]  = "(Q th) entry slip road closed";
    $Event[407]  = "(Q th) exit slip road closed";
    $Event[408]  = "slip roads closed";
    $Event[409]  = "slip road restrictions";
    $Event[410]  = "closed ahead. Stationary traffic";
    $Event[411]  = "closed ahead. Stationary traffic for km";
    $Event[412]  = "closed ahead. Stationary traffic for km";
    $Event[413]  = "closed ahead. Stationary traffic for km";
    $Event[414]  = "closed ahead. Stationary traffic for km";
    $Event[415]  = "closed ahead. Stationary traffic for km";
    $Event[416]  = "closed ahead. Danger of stationary traffic";
    $Event[417]  = "closed ahead. Queuing traffic";
    $Event[418]  = "closed ahead. Queuing traffic for km";
    $Event[419]  = "closed ahead. Queuing traffic for km";
    $Event[420]  = "closed ahead. Queuing traffic for km";
    $Event[421]  = "closed ahead. Queuing traffic for km";
    $Event[422]  = "closed ahead. Queuing traffic for km";
    $Event[423]  = "closed ahead. Danger of queuing traffic";
    $Event[424]  = "closed ahead. Slow traffic";
    $Event[425]  = "closed ahead. Slow traffic for km";
    $Event[426]  = "closed ahead. Slow traffic for km";
    $Event[427]  = "closed ahead. Slow traffic for km";
    $Event[428]  = "closed ahead. Slow traffic for km";
    $Event[429]  = "closed ahead. Slow traffic for km";
    $Event[430]  = "closed ahead. Slow traffic expected";
    $Event[431]  = "closed ahead. Heavy traffic";
    $Event[432]  = "closed ahead. Heavy traffic expected";
    $Event[433]  = "closed ahead. Traffic flowing freely";
    $Event[434]  = "closed ahead. Traffic building up";
    $Event[435]  = "closed ahead. Delays (Q)";
    $Event[436]  = "closed ahead. Delays (Q) expected";
    $Event[437]  = "closed ahead. Long delays (Q)";
    $Event[438]  = "blocked ahead. Stationary traffic";
    $Event[439]  = "blocked ahead. Stationary traffic for km";
    $Event[440]  = "blocked ahead. Stationary traffic for km";
    $Event[441]  = "blocked ahead. Stationary traffic for km";
    $Event[442]  = "blocked ahead. Stationary traffic for km";
    $Event[443]  = "blocked ahead. Stationary traffic for km";
    $Event[444]  = "blocked ahead. Danger of stationary traffic";
    $Event[445]  = "blocked ahead. Queuing traffic";
    $Event[446]  = "blocked ahead. Queuing traffic for km";
    $Event[447]  = "blocked ahead. Queuing traffic for km";
    $Event[448]  = "blocked ahead. Queuing traffic for km";
    $Event[449]  = "blocked ahead. Queuing traffic for km";
    $Event[450]  = "blocked ahead. Queuing traffic for km";
    $Event[451]  = "blocked ahead. Danger of queuing traffic";
    $Event[452]  = "blocked ahead. Slow traffic";
    $Event[453]  = "blocked ahead. Slow traffic for km";
    $Event[454]  = "blocked ahead. Slow traffic for km";
    $Event[455]  = "blocked ahead. Slow traffic for km";
    $Event[456]  = "blocked ahead. Slow traffic for km";
    $Event[457]  = "blocked ahead. Slow traffic for km";
    $Event[458]  = "blocked ahead. Slow traffic expected";
    $Event[459]  = "blocked ahead. Heavy traffic";
    $Event[460]  = "blocked ahead. Heavy traffic expected";
    $Event[461]  = "blocked ahead. Traffic flowing freely";
    $Event[462]  = "blocked ahead. Traffic building up";
    $Event[463]  = "blocked ahead. Delays (Q)";
    $Event[464]  = "blocked ahead. Delays (Q) expected";
    $Event[465]  = "blocked ahead. Long delays (Q)";
    $Event[466]  = "slip roads reopened";
    $Event[467]  = "reopened";
    $Event[468]  = "message cancelled";
    $Event[469]  = "closed ahead";
    $Event[470]  = "blocked ahead";
    $Event[471]  = "(Q) entry slip road(s) closed";
    $Event[472]  = "(Q th) entry slip road blocked";
    $Event[473]  = "entry blocked";
    $Event[474]  = "(Q) exit slip road(s) closed";
    $Event[475]  = "(Q th) exit slip road blocked";
    $Event[476]  = "exit blocked";
    $Event[477]  = "slip roads blocked";
    $Event[478]  = "connecting carriageway closed";
    $Event[479]  = "parallel carriageway closed";
    $Event[480]  = "right-hand parallel carriageway closed";
    $Event[481]  = "left-hand parallel carriageway closed";
    $Event[482]  = "express lanes closed";
    $Event[483]  = "through traffic lanes closed";
    $Event[484]  = "local lanes closed";
    $Event[485]  = "connecting carriageway blocked";
    $Event[486]  = "parallel carriageway blocked";
    $Event[487]  = "right-hand parallel carriageway blocked";
    $Event[488]  = "left-hand parallel carriageway blocked";
    $Event[489]  = "express lanes blocked";
    $Event[490]  = "through traffic lanes blocked";
    $Event[491]  = "local lanes blocked";
    $Event[492]  = "no motor vehicles";
    $Event[493]  = "restrictions";
    $Event[494]  = "closed for heavy lorries (over Q)";
    $Event[495]  = "closed ahead. Stationary traffic for km";
    $Event[496]  = "closed ahead. Queuing traffic for km";
    $Event[497]  = "closed ahead. Slow traffic for km";
    $Event[498]  = "blocked ahead. Stationary traffic for km";
    $Event[499]  = "blocked ahead. Queuing traffic for km";
    $Event[500]  = "(Q) lane(s) closed";
    $Event[501]  = "(Q) right lane(s) closed";
    $Event[502]  = "(Q) centre lane(s) closed";
    $Event[503]  = "(Q) left lane(s) closed";
    $Event[504]  = "hard shoulder closed";
    $Event[505]  = "two lanes closed";
    $Event[506]  = "three lanes closed";
    $Event[507]  = "(Q) right lane(s) blocked";
    $Event[508]  = "(Q) centre lane(s) blocked";
    $Event[509]  = "(Q) left lane(s) blocked";
    $Event[510]  = "hard shoulder blocked";
    $Event[511]  = "two lanes blocked";
    $Event[512]  = "three lanes blocked";
    $Event[513]  = "single alternate line traffic";
    $Event[514]  = "carriageway reduced (from Q lanes) to one lane";
    $Event[515]  = "carriageway reduced (from Q lanes) to two lanes";
    $Event[516]  = "carriageway reduced (from Q lanes) to three lanes";
    $Event[517]  = "contraflow";
    $Event[518]  = "narrow lanes";
    $Event[519]  = "contraflow with narrow lanes";
    $Event[520]  = "(Q) lane(s) blocked";
    $Event[521]  = "(Q) lanes closed. Stationary traffic";
    $Event[522]  = "(Q) lanes closed. Stationary traffic for km";
    $Event[523]  = "(Q) lanes closed. Stationary traffic for km";
    $Event[524]  = "(Q) lanes closed. Stationary traffic for km";
    $Event[525]  = "(Q) lanes closed. Stationary traffic for km";
    $Event[526]  = "(Q) lanes closed. Stationary traffic for km";
    $Event[527]  = "(Q) lanes closed. Danger of stationary traffic";
    $Event[528]  = "(Q) lanes closed. Queuing traffic";
    $Event[529]  = "(Q) lanes closed. Queuing traffic for km";
    $Event[530]  = "(Q) lanes closed. Queuing traffic for km";
    $Event[531]  = "(Q) lanes closed. Queuing traffic for km";
    $Event[532]  = "(Q) lanes closed. Queuing traffic for km";
    $Event[533]  = "(Q) lanes closed. Queuing traffic for km";
    $Event[534]  = "(Q) lanes closed. Danger of queuing traffic";
    $Event[535]  = "(Q) lanes closed. Slow traffic";
    $Event[536]  = "(Q) lanes closed. Slow traffic for km";
    $Event[537]  = "(Q) lanes closed. Slow traffic for km";
    $Event[538]  = "(Q) lanes closed. Slow traffic for km";
    $Event[539]  = "(Q) lanes closed. Slow traffic for km";
    $Event[540]  = "(Q) lanes closed. Slow traffic for km";
    $Event[541]  = "(Q) lanes closed. Slow traffic expected";
    $Event[542]  = "(Q) lanes closed. Heavy traffic";
    $Event[543]  = "(Q) lanes closed. Heavy traffic expected";
    $Event[544]  = "(Q)lanes closed. Traffic flowing freely";
    $Event[545]  = "(Q)lanes closed. Traffic building up";
    $Event[546]  = "carriageway reduced (from Q lanes) to one lane. Stationary traffic";
    $Event[547]  = "carriageway reduced (from Q lanes) to one lane. Danger of stationary traffic";
    $Event[548]  = "carriageway reduced (from Q lanes) to one lane. Queuing traffic";
    $Event[549]  = "carriageway reduced (from Q lanes) to one lane. Danger of queuing traffic";
    $Event[550]  = "carriageway reduced (from Q lanes) to one lane. Slow traffic";
    $Event[551]  = "carriageway reduced (from Q lanes) to one lane. Slow traffic expected";
    $Event[552]  = "carriageway reduced (from Q lanes) to one lane. Heavy traffic";
    $Event[553]  = "carriageway reduced (from Q lanes) to one lane. Heavy traffic expected";
    $Event[554]  = "carriageway reduced (from Q lanes) to one lane. Traffic flowing freely";
    $Event[555]  = "carriageway reduced (from Q lanes) to one lane. Traffic building up";
    $Event[556]  = "carriageway reduced (from Q lanes) to two lanes. Stationary traffic";
    $Event[557]  = "carriageway reduced (from Q lanes) to two lanes. Danger of stationary traffi";
    $Event[558]  = "carriageway reduced (from Q lanes) to two lanes. Queuing traffic";
    $Event[559]  = "carriageway reduced (from Q lanes) to two lanes. Danger of queuing traffic";
    $Event[560]  = "carriageway reduced (from Q lanes) to two lanes. Slow traffic";
    $Event[561]  = "carriageway reduced (from Q lanes) to two lanes. Slow traffic expected";
    $Event[562]  = "carriageway reduced (from Q lanes) to two lanes. Heavy traffic";
    $Event[563]  = "carriageway reduced (from Q lanes) to two lanes. Heavy traffic expected";
    $Event[564]  = "carriageway reduced (from Q lanes) to two lanes. Traffic flowing freely";
    $Event[565]  = "carriageway reduced (from Q lanes) to two lanes. Traffic building up";
    $Event[566]  = "carriageway reduced (from Q lanes) to three lanes. Stationary traffic";
    $Event[567]  = "carriageway reduced (from Q lanes) to three lanes. Danger of stationary traf";
    $Event[568]  = "carriageway reduced (from Q lanes) to three lanes. Queuing traffic";
    $Event[569]  = "carriageway reduced (from Q lanes) to three lanes. Danger of queuing traffic";
    $Event[570]  = "carriageway reduced (from Q lanes) to three lanes. Slow traffic";
    $Event[571]  = "carriageway reduced (from Q lanes) to three lanes. Slow traffic expected";
    $Event[572]  = "carriageway reduced (from Q lanes) to three lanes. Heavy traffic";
    $Event[573]  = "carriageway reduced (from Q lanes) to three lanes. Heavy traffic expected";
    $Event[574]  = "carriageway reduced (from Q lanes) to three lanes. Traffic flowing freely";
    $Event[575]  = "carriageway reduced (from Q lanes) to three lanes. Traffic building up";
    $Event[576]  = "contraflow. Stationary traffic";
    $Event[577]  = "contraflow. Stationary traffic for km";
    $Event[578]  = "contraflow. Stationary traffic for km";
    $Event[579]  = "contraflow. Stationary traffic for km";
    $Event[580]  = "contraflow. Stationary traffic for km";
    $Event[581]  = "contraflow. Stationary traffic for km";
    $Event[582]  = "contraflow. Danger of stationary traffic";
    $Event[583]  = "contraflow. Queuing traffic";
    $Event[584]  = "contraflow. Queuing traffic for km";
    $Event[585]  = "contraflow. Queuing traffic for km";
    $Event[586]  = "contraflow. Queuing traffic for km";
    $Event[587]  = "contraflow. Queuing traffic for km";
    $Event[588]  = "contraflow. Queuing traffic for km";
    $Event[589]  = "contraflow. Danger of queuing traffic";
    $Event[590]  = "contraflow. Slow traffic";
    $Event[591]  = "contraflow. Slow traffic for km";
    $Event[592]  = "contraflow. Slow traffic for km";
    $Event[593]  = "contraflow. Slow traffic for km";
    $Event[594]  = "contraflow. Slow traffic for km";
    $Event[595]  = "contraflow. Slow traffic for km";
    $Event[596]  = "contraflow. Slow traffic expected";
    $Event[597]  = "contraflow. Heavy traffic";
    $Event[598]  = "contraflow. Heavy traffic expected";
    $Event[599]  = "contraflow. Traffic flowing freely";
    $Event[600]  = "contraflow. Traffic building up";
    $Event[601]  = "contraflow. Carriageway reduced (from Q lanes) to one lane";
    $Event[602]  = "contraflow. Carriageway reduced (from Q lanes) to two lanes";
    $Event[603]  = "contraflow. Carriageway reduced (from Q lanes) to three lanes";
    $Event[604]  = "narrow lanes. Stationary traffic";
    $Event[605]  = "narrow lanes. Danger of stationary traffic";
    $Event[606]  = "narrow lanes. Queuing traffic";
    $Event[607]  = "narrow lanes. Danger of queuing traffic";
    $Event[608]  = "narrow lanes. Slow traffic";
    $Event[609]  = "narrow lanes. Slow traffic expected";
    $Event[610]  = "narrow lanes. Heavy traffic";
    $Event[611]  = "narrow lanes. Heavy traffic expected";
    $Event[612]  = "narrow lanes. Traffic flowing freely";
    $Event[613]  = "narrow lanes. Traffic building up";
    $Event[614]  = "contraflow with narrow lanes. Stationary traffic";
    $Event[615]  = "contraflow with narrow lanes. Stationary traffic. Danger of stationary traffic";
    $Event[616]  = "contraflow with narrow lanes. Queuing traffic";
    $Event[617]  = "contraflow with narrow lanes. Danger of queuing traffic";
    $Event[618]  = "contraflow with narrow lanes. Slow traffic";
    $Event[619]  = "contraflow with narrow lanes. Slow traffic expected";
    $Event[620]  = "contraflow with narrow lanes. Heavy traffic";
    $Event[621]  = "contraflow with narrow lanes. Heavy traffic expected";
    $Event[622]  = "contraflow with narrow lanes. Traffic flowing freely";
    $Event[623]  = "contraflow with narrow lanes. Traffic building up";
    $Event[624]  = "lane closures removed";
    $Event[625]  = "message cancelled";
    $Event[626]  = "blocked ahead. Slow traffic for km";
    $Event[627]  = "no motor vehicles without catalytic converters";
    $Event[628]  = "no motor vehicles with even-numbered registration plates";
    $Event[629]  = "no motor vehicles with odd-numbered registration plates";
    $Event[630]  = "open";
    $Event[631]  = "road cleared";
    $Event[632]  = "entry reopened";
    $Event[633]  = "exit reopened";
    $Event[634]  = "all carriageways reopened";
    $Event[635]  = "motor vehicle restrictions lifted";
    $Event[636]  = "traffic restrictions lifted {reopened for all traffic}";
    $Event[637]  = "emergency lane closed";
    $Event[638]  = "turning lane closed";
    $Event[639]  = "crawler lane closed";
    $Event[640]  = "slow vehicle lane closed";
    $Event[641]  = "one lane closed";
    $Event[642]  = "emergency lane blocked";
    $Event[643]  = "turning lane blocked";
    $Event[644]  = "crawler lane blocked";
    $Event[645]  = "slow vehicle lane blocked";
    $Event[646]  = "one lane blocked";
    $Event[647]  = "(Q person) carpool lane in operation";
    $Event[648]  = "(Q person) carpool lane closed";
    $Event[649]  = "(Q person) carpool lane blocked";
    $Event[650]  = "carpool restrictions changed (to Q persons per vehicle)";
    $Event[651]  = "(Q) lanes closed. Stationary traffic for km";
    $Event[652]  = "(Q) lanes closed. Queuing traffic for km";
    $Event[653]  = "(Q) lanes closed. Slow traffic for km";
    $Event[654]  = "contraflow. Stationary traffic for km";
    $Event[655]  = "contraflow. Queuing traffic for km";
    $Event[656]  = "contraflow. Slow traffic for km";
    $Event[657]  = "lane blockages cleared";
    $Event[658]  = "contraflow removed";
    $Event[659]  = "(Q person) carpool restrictions lifted";
    $Event[660]  = "lane restrictions lifted";
    $Event[661]  = "use of hard shoulder allowed";
    $Event[662]  = "normal lane regulations restored";
    $Event[663]  = "all carriageways cleared";
    $Event[671]  = "bus lane available for carpools (with at least Q occupants)";
    $Event[672]  = "message cancelled";
    $Event[673]  = "message cancelled";
    $Event[676]  = "bus lane blocked";
    $Event[678]  = "heavy vehicle lane closed";
    $Event[679]  = "heavy vehicle lane blocked";
    $Event[680]  = "reopened for through traffic";
    $Event[701]  = "(Q sets of) roadworks";
    $Event[702]  = "(Q sets of) major roadworks";
    $Event[703]  = "(Q sets of) maintenance work";
    $Event[704]  = "(Q sections of) resurfacing work";
    $Event[705]  = "(Q sets of) central reservation work";
    $Event[706]  = "(Q sets of) road marking work";
    $Event[707]  = "bridge maintenance work (at Q bridges)";
    $Event[708]  = "(Q sets of) temporary traffic lights";
    $Event[709]  = "(Q sections of) blasting work";
    $Event[710]  = "(Q sets of) roadworks. Stationary traffic";
    $Event[711]  = "(Q sets of) roadworks. Stationary traffic for km";
    $Event[712]  = "(Q sets of) roadworks. Stationary traffic for km";
    $Event[713]  = "(Q sets of) roadworks. Stationary traffic for km";
    $Event[714]  = "(Q sets of) roadworks. Stationary traffic for km";
    $Event[715]  = "(Q sets of) roadworks. Stationary traffic for km";
    $Event[716]  = "(Q sets of) roadworks. Danger of stationary traffic";
    $Event[717]  = "(Q sets of) roadworks. Queuing traffic";
    $Event[718]  = "(Q sets of) roadworks. Queuing traffic for km";
    $Event[719]  = "(Q sets of) roadworks. Queuing traffic for km";
    $Event[720]  = "(Q sets of) roadworks. Queuing traffic for km";
    $Event[721]  = "(Q sets of) roadworks. Queuing traffic for km";
    $Event[722]  = "(Q sets of) roadworks. Queuing traffic for km";
    $Event[723]  = "(Q sets of) roadworks. Danger of queuing traffic";
    $Event[724]  = "(Q sets of) roadworks. Slow traffic";
    $Event[725]  = "(Q sets of) roadworks. Slow traffic for km";
    $Event[726]  = "(Q sets of) roadworks. Slow traffic for km";
    $Event[727]  = "(Q sets of) roadworks. Slow traffic for km";
    $Event[728]  = "(Q sets of) roadworks. Slow traffic for km";
    $Event[729]  = "(Q sets of) roadworks. Slow traffic for km";
    $Event[730]  = "(Q sets of) roadworks. Slow traffic expected";
    $Event[731]  = "(Q sets of) roadworks. Heavy traffic";
    $Event[732]  = "(Q sets of) roadworks. Heavy traffic expected";
    $Event[733]  = "(Q sets of) roadworks. Traffic flowing freely";
    $Event[734]  = "(Q sets of) roadworks. Traffic building up";
    $Event[735]  = "closed due to (Q sets of) roadworks";
    $Event[736]  = "(Q sets of) roadworks. Right lane closed";
    $Event[737]  = "(Q sets of) roadworks. Centre lane closed";
    $Event[738]  = "(Q sets of) roadworks. Left lane closed";
    $Event[739]  = "(Q sets of) roadworks. Hard shoulder closed";
    $Event[740]  = "(Q sets of) roadworks. Two lanes closed";
    $Event[741]  = "(Q sets of) roadworks. Three lanes closed";
    $Event[742]  = "(Q sets of) roadworks. Single alternate line traffic";
    $Event[743]  = "roadworks. Carriageway reduced (from Q lanes) to one lane";
    $Event[744]  = "roadworks. Carriageway reduced (from Q lanes) to two lanes";
    $Event[745]  = "roadworks. Carriageway reduced (from Q lanes) to three lanes";
    $Event[746]  = "(Q sets of) roadworks. Contraflow";
    $Event[747]  = "roadworks. Delays (Q)";
    $Event[748]  = "roadworks. Delays (Q) expected";
    $Event[749]  = "roadworks. Long delays (Q)";
    $Event[750]  = "(Q sections of) resurfacing work. Stationary traffic";
    $Event[751]  = "(Q sections of) resurfacing work. Stationary traffic for km";
    $Event[752]  = "(Q sections of) resurfacing work. Stationary traffic for km";
    $Event[753]  = "(Q sections of) resurfacing work. Stationary traffic for km";
    $Event[754]  = "(Q sections of) resurfacing work. Stationary traffic for km";
    $Event[755]  = "(Q sections of) resurfacing work. Stationary traffic for km";
    $Event[756]  = "(Q sections of) resurfacing work. Danger of stationary traffic";
    $Event[757]  = "(Q sections of) resurfacing work. Queuing traffic";
    $Event[758]  = "(Q sections of) resurfacing work. Queuing traffic for km";
    $Event[759]  = "(Q sections of) resurfacing work. Queuing traffic for km";
    $Event[760]  = "(Q sections of) resurfacing work. Queuing traffic for km";
    $Event[761]  = "(Q sections of) resurfacing work. Queuing traffic for km";
    $Event[762]  = "(Q sections of) resurfacing work. Queuing traffic for km";
    $Event[763]  = "(Q sections of) resurfacing work. Danger of queuing traffic";
    $Event[764]  = "(Q sections of) resurfacing work. Slow traffic";
    $Event[765]  = "(Q sections of) resurfacing work. Slow traffic for km";
    $Event[766]  = "(Q sections of) resurfacing work. Slow traffic for km";
    $Event[767]  = "(Q sections of) resurfacing work. Slow traffic for km";
    $Event[768]  = "(Q sections of) resurfacing work. Slow traffic for km";
    $Event[769]  = "(Q sections of) resurfacing work. Slow traffic for km";
    $Event[770]  = "(Q sections of) resurfacing work. Slow traffic expected";
    $Event[771]  = "(Q sections of) resurfacing work. Heavy traffic";
    $Event[772]  = "(Q sections of) resurfacing work. Heavy traffic expected";
    $Event[773]  = "(Q sections of) resurfacing work. Traffic flowing freely";
    $Event[774]  = "(Q sections of) resurfacing work. Traffic building up";
    $Event[775]  = "(Q sections of) resurfacing work. Single alternate line traffic";
    $Event[776]  = "resurfacing work. Carriageway reduced (from Q lanes) to one lane";
    $Event[777]  = "resurfacing work. Carriageway reduced (from Q lanes) to two lanes";
    $Event[778]  = "resurfacing work. Carriageway reduced (from Q lanes) to three lanes";
    $Event[779]  = "(Q sections of) resurfacing work. Contraflow";
    $Event[780]  = "resurfacing work. Delays (Q)";
    $Event[781]  = "resurfacing work. Delays (Q) expected";
    $Event[782]  = "resurfacing work. Long delays (Q)";
    $Event[783]  = "(Q sets of) road marking work. Stationary traffic";
    $Event[784]  = "(Q sets of) road marking work. Danger of stationary traffic";
    $Event[785]  = "(Q sets of) road marking work. Queuing traffic";
    $Event[786]  = "(Q sets of) road marking work. Danger of queuing traffic";
    $Event[787]  = "(Q sets of) road marking work. Slow traffic";
    $Event[788]  = "(Q sets of) road marking work. Slow traffic expected";
    $Event[789]  = "(Q sets of) road marking work. Heavy traffic";
    $Event[790]  = "(Q sets of) road marking work. Heavy traffic expected";
    $Event[791]  = "(Q sets of) road marking work. Traffic flowing freely";
    $Event[792]  = "(Q sets of) road marking work. Traffic building up";
    $Event[793]  = "(Q sets of) road marking work. Right lane closed";
    $Event[794]  = "(Q sets of) road marking work. Centre lane closed";
    $Event[795]  = "(Q sets of) road marking work. Left lane closed";
    $Event[796]  = "(Q sets of) road marking work. Hard shoulder closed";
    $Event[797]  = "(Q sets of) road marking work. Two lanes closed";
    $Event[798]  = "(Q sets of) road marking work. Three lanes closed";
    $Event[799]  = "closed for bridge demolition work (at Q bridges)";
    $Event[800]  = "roadworks cleared";
    $Event[801]  = "message cancelled";
    $Event[802]  = "(Q sets of) long-term roadworks";
    $Event[803]  = "(Q sets of) construction work";
    $Event[804]  = "(Q sets of) slow moving maintenance vehicles";
    $Event[805]  = "bridge demolition work (at Q bridges)";
    $Event[806]  = "(Q sets of) water main work";
    $Event[807]  = "(Q sets of) gas main work";
    $Event[808]  = "(Q sets of) work on buried cables";
    $Event[809]  = "(Q sets of) work on buried services";
    $Event[810]  = "new roadworks layout";
    $Event[811]  = "new road layout";
    $Event[812]  = "(Q sets of) roadworks. Stationary traffic for km";
    $Event[813]  = "(Q sets of) roadworks. Queuing traffic for km";
    $Event[814]  = "(Q sets of) roadworks. Slow traffic for km";
    $Event[815]  = "(Q sets of) roadworks during the day time";
    $Event[816]  = "(Q sets of) roadworks during off-peak periods";
    $Event[817]  = "(Q sets of) roadworks during the night";
    $Event[818]  = "(Q sections of) resurfacing work. Stationary traffic for km";
    $Event[819]  = "(Q sections of) resurfacing work. Queuing traffic for km";
    $Event[820]  = "(Q sections of) resurfacing work. Slow traffic for km";
    $Event[821]  = "(Q sets of) resurfacing work during the day time";
    $Event[822]  = "(Q sets of) resurfacing work during off-peak periods";
    $Event[823]  = "(Q sets of) resurfacing work during the night";
    $Event[824]  = "(Q sets of) road marking work. Danger";
    $Event[825]  = "(Q sets of) slow moving maintenance vehicles. Stationary traffic";
    $Event[826]  = "(Q sets of) slow moving maintenance vehicles. Danger of stationary traffic";
    $Event[827]  = "(Q sets of) slow moving maintenance vehicles. Queuing traffic";
    $Event[828]  = "(Q sets of) slow moving maintenance vehicles. Danger of queuing traffic";
    $Event[829]  = "(Q sets of) slow moving maintenance vehicles. Slow traffic";
    $Event[830]  = "(Q sets of) slow moving maintenance vehicles. Slow traffic expected";
    $Event[831]  = "(Q sets of) slow moving maintenance vehicles. Heavy traffic";
    $Event[832]  = "(Q sets of) slow moving maintenance vehicles. Heavy traffic expected";
    $Event[833]  = "(Q sets of) slow moving maintenance vehicles. Traffic flowing freely";
    $Event[834]  = "(Q sets of) slow moving maintenance vehicles. Traffic building up";
    $Event[835]  = "(Q sets of) slow moving maintenance vehicles. Right lane closed";
    $Event[836]  = "(Q sets of) slow moving maintenance vehicles. Centre lane closed";
    $Event[837]  = "(Q sets of) slow moving maintenance vehicles. Left lane closed";
    $Event[838]  = "(Q sets of) slow moving maintenance vehicles. Two lanes closed";
    $Event[839]  = "(Q sets of) slow moving maintenance vehicles. Three lanes closed";
    $Event[840]  = "water main work. Delays (Q)";
    $Event[841]  = "water main work. Delays (Q) expected";
    $Event[842]  = "water main work. Long delays (Q)";
    $Event[843]  = "gas main work. Delays (Q)";
    $Event[844]  = "gas main work. Delays (Q) expected";
    $Event[845]  = "gas main work. Long delays (Q)";
    $Event[846]  = "work on buried cables. Delays (Q)";
    $Event[847]  = "work on buried cables. Delays (Q) expected";
    $Event[848]  = "work on buried cables. Long delays (Q)";
    $Event[849]  = "work on buried services. Delays (Q)";
    $Event[850]  = "work on buried services. Delays (Q) expected";
    $Event[851]  = "work on buried services. Long delays (Q)";
    $Event[852]  = "construction traffic merging";
    $Event[853]  = "roadwork clearance in progress";
    $Event[854]  = "maintenance work cleared";
    $Event[855]  = "road layout unchanged";
    $Event[856]  = "construction traffic merging. Danger";
    $Event[898]  = "obstruction warning withdrawn";
    $Event[899]  = "clearance work in progress, road free again";
    $Event[900]  = "flooding expected";
    $Event[901]  = "(Q) obstruction(s) on roadway {something that does block the road or part o";
    $Event[902]  = "(Q) obstructions on the road. Danger";
    $Event[903]  = "spillage on the road";
    $Event[904]  = "storm damage";
    $Event[905]  = "(Q) fallen trees";
    $Event[906]  = "(Q) fallen trees. Danger";
    $Event[907]  = "flooding";
    $Event[908]  = "flooding. Danger";
    $Event[909]  = "flash floods";
    $Event[910]  = "danger of flash floods";
    $Event[911]  = "avalanches";
    $Event[912]  = "avalanche risk";
    $Event[913]  = "rockfalls";
    $Event[914]  = "landslips";
    $Event[915]  = "earthquake damage";
    $Event[916]  = "road surface in poor condition";
    $Event[917]  = "subsidence";
    $Event[918]  = "(Q) collapsed sewer(s)";
    $Event[919]  = "burst water main";
    $Event[920]  = "gas leak";
    $Event[921]  = "serious fire";
    $Event[922]  = "animals on roadway";
    $Event[923]  = "animals on the road. Danger";
    $Event[924]  = "clearance work";
    $Event[925]  = "blocked by storm damage";
    $Event[926]  = "blocked by (Q) fallen trees";
    $Event[927]  = "(Q) fallen tree(s). Passable with care";
    $Event[928]  = "flooding. Stationary traffic";
    $Event[929]  = "flooding. Danger of stationary traffic";
    $Event[930]  = "flooding. Queuing traffic";
    $Event[931]  = "flooding. Danger of queuing traffic";
    $Event[932]  = "flooding. Slow traffic";
    $Event[933]  = "flooding. Slow traffic expected";
    $Event[934]  = "flooding. Heavy traffic";
    $Event[935]  = "flooding. Heavy traffic expected";
    $Event[936]  = "flooding. Traffic flowing freely";
    $Event[937]  = "flooding. Traffic building up";
    $Event[938]  = "closed due to flooding";
    $Event[939]  = "flooding. Delays (Q)";
    $Event[940]  = "flooding. Delays (Q) expected";
    $Event[941]  = "flooding. Long delays (Q)";
    $Event[942]  = "flooding. Passable with care";
    $Event[943]  = "closed due to avalanches";
    $Event[944]  = "avalanches. Passable with care (above Q hundred metres)";
    $Event[945]  = "closed due to rockfalls";
    $Event[946]  = "rockfalls. Passable with care";
    $Event[947]  = "road closed due to landslips";
    $Event[948]  = "landslips. Passable with care";
    $Event[949]  = "closed due to subsidence";
    $Event[950]  = "subsidence. Single alternate line traffic";
    $Event[951]  = "subsidence. Carriageway reduced (from Q lanes) to one lane";
    $Event[952]  = "subsidence. Carriageway reduced (from Q lanes) to two lanes";
    $Event[953]  = "subsidence. Carriageway reduced (from Q lanes) to three lanes";
    $Event[954]  = "subsidence. Contraflow in operation";
    $Event[955]  = "subsidence. Passable with care";
    $Event[956]  = "closed due to sewer collapse";
    $Event[957]  = "road closed due to burst water main";
    $Event[958]  = "burst water main. Delays (Q)";
    $Event[959]  = "burst water main. Delays (Q) expected";
    $Event[960]  = "burst water main. Long delays (Q)";
    $Event[961]  = "closed due to gas leak";
    $Event[962]  = "gas leak. Delays (Q)";
    $Event[963]  = "gas leak. Delays (Q) expected";
    $Event[964]  = "gas leak. Long delays (Q)";
    $Event[965]  = "closed due to serious fire";
    $Event[966]  = "serious fire. Delays (Q)";
    $Event[967]  = "serious fire. Delays (Q) expected";
    $Event[968]  = "serious fire. Long delays (Q)";
    $Event[969]  = "closed for clearance work";
    $Event[970]  = "road free again";
    $Event[971]  = "message cancelled";
    $Event[972]  = "storm damage expected";
    $Event[973]  = "fallen power cables";
    $Event[974]  = "sewer overflow";
    $Event[975]  = "ice build-up";
    $Event[976]  = "mud slide";
    $Event[977]  = "grass fire";
    $Event[978]  = "air crash";
    $Event[979]  = "rail crash";
    $Event[980]  = "blocked by (Q) obstruction(s) on the road";
    $Event[981]  = "(Q) obstructions on the road. Passable with care";
    $Event[982]  = "blocked due to spillage on roadway";
    $Event[983]  = "spillage on the road. Passable with care";
    $Event[984]  = "spillage on the road. Danger";
    $Event[985]  = "storm damage. Passable with care";
    $Event[986]  = "storm damage. Danger";
    $Event[987]  = "blocked by fallen power cables";
    $Event[988]  = "fallen power cables. Passable with care";
    $Event[989]  = "fallen power cables. Danger";
    $Event[990]  = "sewer overflow. Danger";
    $Event[991]  = "flash floods. Danger";
    $Event[992]  = "avalanches. Danger";
    $Event[993]  = "closed due to avalanche risk";
    $Event[994]  = "avalanche risk. Danger";
    $Event[995]  = "closed due to ice build-up";
    $Event[996]  = "ice build-up. Passable with care (above Q hundred metres)";
    $Event[997]  = "ice build-up. Single alternate traffic";
    $Event[998]  = "rockfalls. Danger";
    $Event[999]  = "landslips. Danger";
    $Event[1000] = "earthquake damage. Danger";
    $Event[1001] = "hazardous driving conditions (above Q hundred metres)";
    $Event[1002] = "danger of aquaplaning";
    $Event[1003] = "slippery road (above Q hundred metres)";
    $Event[1004] = "mud on road";
    $Event[1005] = "leaves on road";
    $Event[1006] = "ice (above Q hundred metres)";
    $Event[1007] = "danger of ice (above Q hundred metres)";
    $Event[1008] = "black ice (above Q hundred metres)";
    $Event[1009] = "freezing rain (above Q hundred metres)";
    $Event[1010] = "wet and icy roads (above Q hundred metres)";
    $Event[1011] = "slush (above Q hundred metres)";
    $Event[1012] = "snow on the road (above Q hundred metres)";
    $Event[1013] = "packed snow (above Q hundred metres)";
    $Event[1014] = "fresh snow (above Q hundred metres)";
    $Event[1015] = "deep snow (above Q hundred metres)";
    $Event[1016] = "snow drifts (above Q hundred metres)";
    $Event[1017] = "slippery due to spillage on roadway";
    $Event[1018] = "slippery road (above Q hundred metres) due to snow";
    $Event[1019] = "slippery road (above Q hundred metres) due to frost";
    $Event[1020] = "road blocked by snow (above Q hundred metres)";
    $Event[1021] = "snow on the road. Carriageway reduced (from Q lanes) to one lane";
    $Event[1022] = "snow on the road. Carriageway reduced (from Q lanes) to two lanes";
    $Event[1023] = "snow on the road. Carriageway reduced (from Q lanes) to three lanes";
    $Event[1024] = "conditions of road surface improved";
    $Event[1025] = "message cancelled";
    $Event[1026] = "subsidence. Danger";
    $Event[1027] = "sewer collapse. Delays (Q)";
    $Event[1028] = "sewer collapse. Delays (Q) expected";
    $Event[1029] = "sewer collapse. Long delays (Q)";
    $Event[1030] = "sewer collapse. Danger";
    $Event[1031] = "burst water main. Danger";
    $Event[1032] = "gas leak. Danger";
    $Event[1033] = "serious fire. Danger";
    $Event[1034] = "clearance work. Danger";
    $Event[1035] = "impassable (above Q hundred metres)";
    $Event[1036] = "almost impassable (above Q hundred metres)";
    $Event[1037] = "extremely hazardous driving conditions (above Q hundred metres)";
    $Event[1038] = "difficult driving conditions (above Q hundred metres)";
    $Event[1039] = "passable with care (up to Q hundred metres)";
    $Event[1040] = "passable (up to Q hundred metres)";
    $Event[1041] = "surface water hazard";
    $Event[1042] = "loose sand on road";
    $Event[1043] = "loose chippings";
    $Event[1044] = "oil on road";
    $Event[1045] = "petrol on road";
    $Event[1047] = "icy patches (above Q hundred metres)";
    $Event[1048] = "danger of icy patches (above Q hundred metres)";
    $Event[1050] = "danger of black ice (above Q hundred metres)";
    $Event[1054] = "slippery due to loose sand on roadway";
    $Event[1055] = "mud on road. Danger";
    $Event[1056] = "loose chippings. Danger";
    $Event[1057] = "oil on road. Danger";
    $Event[1058] = "petrol on road. Danger";
    $Event[1059] = "road surface in poor condition. Danger";
    $Event[1060] = "icy patches (above Q hundred metres) on bridges";
    $Event[1061] = "danger of icy patches (above Q hundred metres) on bridges";
    $Event[1062] = "icy patches (above Q hundred metres) on bridges, in shaded areas and on s";
    $Event[1063] = "impassable for heavy vehicles (over Q)";
    $Event[1064] = "impassable (above Q hundred metres) for vehicles with trailers";
    $Event[1065] = "driving conditions improved";
    $Event[1066] = "rescue and recovery work in progress. Danger";
    $Event[1067] = "large animals on roadway";
    $Event[1068] = "herds of animals on roadway";
    $Event[1069] = "skid hazard reduced";
    $Event[1070] = "snow cleared";
    $Event[1073] = "extremely hazardous driving conditions expected (above Q hundred meters";
    $Event[1074] = "freezing rain expected (above Q hundred metres)";
    $Event[1075] = "danger of road being blocked by snow (above Q hundred metres)";
    $Event[1079] = "temperature falling rapidly (to Q)";
    $Event[1080] = "extreme heat (up to Q)";
    $Event[1081] = "extreme cold (of Q)";
    $Event[1082] = "less extreme temperatures";
    $Event[1083] = "current temperature (Q)";
    $Event[1101] = "heavy snowfall (Q)";
    $Event[1102] = "heavy snowfall (Q). Visibility reduced to <m";
    $Event[1103] = "heavy snowfall (Q). Visibility reduced to <m";
    $Event[1104] = "snowfall (Q)";
    $Event[1105] = "snowfall (Q). Visibility reduced to <m";
    $Event[1106] = "hail (visibility reduced to Q)";
    $Event[1107] = "sleet (visibility reduced to Q)";
    $Event[1108] = "thunderstorms (visibility reduced to Q)";
    $Event[1109] = "heavy rain (Q)";
    $Event[1110] = "heavy rain (Q). Visibility reduced to <m";
    $Event[1111] = "heavy rain (Q). Visibility reduced to <m";
    $Event[1112] = "rain (Q)";
    $Event[1113] = "rain (Q). Visibility reduced to <m";
    $Event[1114] = "showers (visibility reduced to Q)";
    $Event[1115] = "heavy frost";
    $Event[1116] = "frost";
    $Event[1126] = "weather situation improved";
    $Event[1127] = "message cancelled";
    $Event[1128] = "winter storm (visibility reduced to Q)";
    $Event[1130] = "blizzard (visibility reduced to Q)";
    $Event[1132] = "damaging hail (visibility reduced to Q)";
    $Event[1134] = "heavy snowfall. Visibility reduced (to Q)";
    $Event[1135] = "snowfall. Visibility reduced (to Q)";
    $Event[1136] = "heavy rain. Visibility reduced (to Q)";
    $Event[1137] = "rain. Visibility reduced (to Q)";
    $Event[1170] = "heavy snowfall (Q) expected";
    $Event[1171] = "heavy rain (Q) expected";
    $Event[1172] = "weather expected to improve";
    $Event[1173] = "blizzard (with visibility reduced to Q) expected";
    $Event[1174] = "damaging hail (with visibility reduced to Q) expected";
    $Event[1175] = "reduced visibility (to Q) expected";
    $Event[1176] = "freezing fog expected (with visibility reduced to Q). Danger of slippery roads";
    $Event[1177] = "dense fog (with visibility reduced to Q) expected";
    $Event[1178] = "patchy fog (with visibility reduced to Q) expected";
    $Event[1179] = "visibility expected to improve";
    $Event[1180] = "adverse weather warning withdrawn";
    $Event[1190] = "severe smog";
    $Event[1191] = "severe exhaust pollution";
    $Event[1201] = "tornadoes";
    $Event[1202] = "hurricane force winds (Q)";
    $Event[1203] = "gales (Q)";
    $Event[1204] = "storm force winds (Q)";
    $Event[1205] = "strong winds (Q)";
    $Event[1209] = "gusty winds (Q)";
    $Event[1210] = "crosswinds (Q)";
    $Event[1211] = "strong winds (Q) affecting high-sided vehicles";
    $Event[1212] = "closed for high-sided vehicles due to strong winds (Q)";
    $Event[1213] = "strong winds easing";
    $Event[1214] = "message cancelled";
    $Event[1215] = "restrictions for high-sided vehicles lifted";
    $Event[1217] = "tornado warning ended";
    $Event[1301] = "dense fog (visibility reduced to Q)";
    $Event[1302] = "dense fog. Visibility reduced to <m";
    $Event[1303] = "dense fog. Visibility reduced to <m";
    $Event[1304] = "fog (visibility reduced to Q)";
    $Event[1305] = "fog. Visibility reduced to <m";
    $Event[1307] = "patchy fog (visibility reduced to Q)";
    $Event[1308] = "freezing fog (visibility reduced to Q)";
    $Event[1309] = "smoke hazard (visibility reduced to Q)";
    $Event[1310] = "blowing dust (visibility reduced to Q)";
    $Event[1312] = "snowfall and fog (visibility reduced to Q)";
    $Event[1313] = "visibility improved";
    $Event[1314] = "message cancelled";
    $Event[1318] = "visibility reduced (to Q)";
    $Event[1319] = "visibility reduced to <m";
    $Event[1320] = "visibility reduced to <m";
    $Event[1321] = "visibility reduced to <m";
    $Event[1322] = "white out (visibility reduced to Q)";
    $Event[1323] = "blowing snow (visibility reduced to Q)";
    $Event[1324] = "spray hazard (visibility reduced to Q)";
    $Event[1325] = "low sun glare";
    $Event[1326] = "sandstorms (visibility reduced to Q)";
    $Event[1332] = "smog alert";
    $Event[1337] = "freezing fog (visibility reduced to Q). Slippery roads";
    $Event[1338] = "no motor vehicles due to smog alert";
    $Event[1340] = "swarms of insects (visibility reduced to Q)";
    $Event[1345] = "fog clearing";
    $Event[1346] = "fog forecast withdrawn";
    $Event[1450] = "international sports meeting";
    $Event[1451] = "match";
    $Event[1452] = "tournament";
    $Event[1453] = "athletics meeting";
    $Event[1454] = "ball game";
    $Event[1455] = "boxing tournament";
    $Event[1456] = "bull fight";
    $Event[1457] = "cricket match";
    $Event[1458] = "cycle race";
    $Event[1459] = "football match";
    $Event[1460] = "golf tournament";
    $Event[1461] = "marathon";
    $Event[1462] = "race meeting";
    $Event[1463] = "rugby match";
    $Event[1464] = "show jumping";
    $Event[1465] = "tennis tournament";
    $Event[1466] = "water sports meeting";
    $Event[1467] = "winter sports meeting";
    $Event[1468] = "funfair";
    $Event[1469] = "trade fair";
    $Event[1470] = "procession";
    $Event[1471] = "sightseers obstructing access";
    $Event[1472] = "people on roadway";
    $Event[1473] = "children on roadway";
    $Event[1474] = "cyclists on roadway";
    $Event[1475] = "strike";
    $Event[1476] = "security incident";
    $Event[1477] = "police checkpoint";
    $Event[1478] = "terrorist incident";
    $Event[1479] = "gunfire on roadway, danger";
    $Event[1480] = "civil emergency";
    $Event[1481] = "air raid, danger";
    $Event[1482] = "people on roadway. Danger";
    $Event[1483] = "children on roadway. Danger";
    $Event[1484] = "cyclists on roadway. Danger";
    $Event[1485] = "closed due to security incident";
    $Event[1486] = "security incident. Delays (Q)";
    $Event[1487] = "security incident. Delays (Q) expected";
    $Event[1488] = "security incident. Long delays (Q)";
    $Event[1489] = "police checkpoint. Delays (Q)";
    $Event[1490] = "police checkpoint. Delays (Q) expected";
    $Event[1491] = "police checkpoint. Long delays (Q)";
    $Event[1492] = "security alert withdrawn";
    $Event[1493] = "sports traffic cleared";
    $Event[1494] = "evacuation";
    $Event[1495] = "evacuation. Heavy traffic";
    $Event[1496] = "traffic disruption cleared";
    $Event[1501] = "major event";
    $Event[1502] = "sports event meeting";
    $Event[1503] = "show";
    $Event[1504] = "festival";
    $Event[1505] = "exhibition";
    $Event[1506] = "fair";
    $Event[1507] = "market";
    $Event[1508] = "ceremonial event";
    $Event[1509] = "state occasion";
    $Event[1510] = "parade";
    $Event[1511] = "crowd";
    $Event[1512] = "march";
    $Event[1513] = "demonstration";
    $Event[1514] = "public disturbance";
    $Event[1515] = "security alert";
    $Event[1516] = "bomb alert";
    $Event[1517] = "major event. Stationary traffic";
    $Event[1518] = "major event. Danger of stationary traffic";
    $Event[1519] = "major event. Queuing traffic";
    $Event[1520] = "major event. Danger of queuing traffic";
    $Event[1521] = "major event. Slow traffic";
    $Event[1522] = "major event. Slow traffic expected";
    $Event[1523] = "major event. Heavy traffic";
    $Event[1524] = "major event. Heavy traffic expected";
    $Event[1525] = "major event. Traffic flowing freely";
    $Event[1526] = "major event. Traffic building up";
    $Event[1527] = "closed due to major event";
    $Event[1528] = "major event. Delays (Q)";
    $Event[1529] = "major event. Delays (Q) expected";
    $Event[1530] = "major event. Long delays (Q)";
    $Event[1531] = "sports meeting. Stationary traffic";
    $Event[1532] = "sports meeting. Danger of stationary traffic";
    $Event[1533] = "sports meeting. Queuing traffic";
    $Event[1534] = "sports meeting. Danger of queuing traffic";
    $Event[1535] = "sports meeting. Slow traffic";
    $Event[1536] = "sports meeting. Slow traffic expected";
    $Event[1537] = "sports meeting. Heavy traffic";
    $Event[1538] = "sports meeting. Heavy traffic expected";
    $Event[1539] = "sports meeting. Traffic flowing freely";
    $Event[1540] = "sports meeting. Traffic building up";
    $Event[1541] = "closed due to sports meeting";
    $Event[1542] = "sports meeting. Delays (Q)";
    $Event[1543] = "sports meeting. Delays (Q) expected";
    $Event[1544] = "sports meeting. Long delays (Q)";
    $Event[1545] = "fair. Stationary traffic";
    $Event[1546] = "fair. Danger of stationary traffic";
    $Event[1547] = "fair. Queuing traffic";
    $Event[1548] = "fair. Danger of queuing traffic";
    $Event[1549] = "fair. Slow traffic";
    $Event[1550] = "fair. Slow traffic expected";
    $Event[1551] = "fair. Heavy traffic";
    $Event[1552] = "fair. Heavy traffic expected";
    $Event[1553] = "fair. Traffic flowing freely";
    $Event[1554] = "fair. Traffic building up";
    $Event[1555] = "closed due to fair";
    $Event[1556] = "fair. Delays (Q)";
    $Event[1557] = "fair. Delays (Q) expected";
    $Event[1558] = "fair. Long delays (Q)";
    $Event[1559] = "closed due to parade";
    $Event[1560] = "parade. Delays (Q)";
    $Event[1561] = "parade. Delays (Q) expected";
    $Event[1562] = "parade. Long delays (Q)";
    $Event[1563] = "closed due to strike";
    $Event[1564] = "strike. Delays (Q)";
    $Event[1565] = "strike. Delays (Q) expected";
    $Event[1566] = "strike. Long delays (Q)";
    $Event[1567] = "closed due to demonstration";
    $Event[1568] = "demonstration. Delays (Q)";
    $Event[1569] = "demonstration. Delays (Q) expected";
    $Event[1570] = "demonstration. Long delays (Q)";
    $Event[1571] = "security alert. Stationary traffic";
    $Event[1572] = "security alert. Danger of stationary traffic";
    $Event[1573] = "security alert. Queuing traffic";
    $Event[1574] = "security alert. Danger of queuing traffic";
    $Event[1575] = "security alert. Slow traffic";
    $Event[1576] = "security alert. Slow traffic expected";
    $Event[1577] = "security alert. Heavy traffic";
    $Event[1578] = "security alert. Heavy traffic expected";
    $Event[1579] = "security alert. Traffic building up";
    $Event[1580] = "closed due to security alert";
    $Event[1581] = "security alert. Delays (Q)";
    $Event[1582] = "security alert. Delays (Q) expected";
    $Event[1583] = "security alert. Long delays (Q)";
    $Event[1584] = "traffic has returned to normal";
    $Event[1585] = "message cancelled";
    $Event[1586] = "security alert. Traffic flowing freely";
    $Event[1587] = "air raid warning cancelled";
    $Event[1588] = "civil emergency cancelled";
    $Event[1589] = "message cancelled";
    $Event[1590] = "several major events";
    $Event[1591] = "information about major event no longer valid";
    $Event[1601] = "delays (Q)";
    $Event[1602] = "delays up to minutes";
    $Event[1603] = "delays up to minutes";
    $Event[1604] = "delays up to one hour";
    $Event[1605] = "delays up to two hours";
    $Event[1606] = "delays of several hours";
    $Event[1607] = "delays (Q) expected";
    $Event[1608] = "long delays (Q)";
    $Event[1609] = "delays (Q) for heavy vehicles";
    $Event[1610] = "delays up to minutes for heavy lorr(y/ies)";
    $Event[1611] = "delays up to minutes for heavy lorr(y/ies)";
    $Event[1612] = "delays up to one hour for heavy lorr(y/ies)";
    $Event[1613] = "delays up to two hours for heavy lorr(y/ies)";
    $Event[1614] = "delays of several hours for heavy lorr(y/ies)";
    $Event[1615] = "service suspended (until Q)";
    $Event[1616] = "(Q) service withdrawn";
    $Event[1617] = "(Q) service(s) fully booked";
    $Event[1618] = "(Q) service(s) fully booked for heavy vehicles";
    $Event[1619] = "normal services resumed";
    $Event[1620] = "message cancelled";
    $Event[1621] = "delays up to minutes";
    $Event[1622] = "delays up to minutes";
    $Event[1623] = "delays up to minutes";
    $Event[1624] = "delays up to minutes";
    $Event[1625] = "delays up to minutes";
    $Event[1626] = "delays up to minutes";
    $Event[1627] = "delays up to minutes";
    $Event[1628] = "delays up to three hours";
    $Event[1629] = "delays up to four hours";
    $Event[1630] = "delays up to five hours";
    $Event[1631] = "very long delays (Q)";
    $Event[1632] = "delays of uncertain duration";
    $Event[1633] = "delayed until further notice";
    $Event[1634] = "cancellations";
    $Event[1635] = "park and ride service not operating (until Q)";
    $Event[1636] = "special public transport services operating (until Q)";
    $Event[1637] = "normal services not operating (until Q)";
    $Event[1638] = "rail services not operating (until Q)";
    $Event[1639] = "bus services not operating (until Q)";
    $Event[1640] = "shuttle service operating (until Q)";
    $Event[1641] = "free shuttle service operating (until Q)";
    $Event[1642] = "delays (Q) for heavy lorr(y/ies)";
    $Event[1643] = "delays (Q) for buses";
    $Event[1644] = "(Q) service(s) fully booked for heavy lorr(y/ies)";
    $Event[1645] = "(Q) service(s) fully booked for buses";
    $Event[1646] = "next departure (Q) for heavy lorr(y/ies)";
    $Event[1647] = "next departure (Q) for buses";
    $Event[1648] = "delays cleared";
    $Event[1649] = "rapid transit service not operating (until Q)";
    $Event[1650] = "delays (Q) possible";
    $Event[1651] = "underground service not operating (until Q)";
    $Event[1652] = "cancellations expected";
    $Event[1653] = "long delays expected";
    $Event[1654] = "very long delays expected";
    $Event[1655] = "all services fully booked (until Q)";
    $Event[1656] = "next arrival (Q)";
    $Event[1657] = "rail services irregular. Delays (Q)";
    $Event[1658] = "bus services irregular. Delays (Q)";
    $Event[1659] = "underground services irregular";
    $Event[1660] = "normal public transport services resumed";
    $Event[1661] = "ferry service not operating (until Q)";
    $Event[1662] = "park and ride trip time (Q)";
    $Event[1663] = "delay expected to be cleared";
    $Event[1695] = "current trip time (Q)";
    $Event[1696] = "expected trip time (Q)";
    $Event[1700] = "(Q) slow moving maintenance vehicle(s)";
    $Event[1701] = "(Q) vehicle(s) on wrong carriageway";
    $Event[1702] = "dangerous vehicle warning cleared";
    $Event[1703] = "message cancelled";
    $Event[1704] = "(Q) reckless driver(s)";
    $Event[1705] = "(Q) prohibited vehicle(s) on the roadway";
    $Event[1706] = "(Q) emergency vehicles";
    $Event[1707] = "(Q) high-speed emergency vehicles";
    $Event[1708] = "high-speed chase (involving Q vehicles)";
    $Event[1709] = "spillage occurring from moving vehicle";
    $Event[1710] = "objects falling from moving vehicle";
    $Event[1711] = "emergency vehicle warning cleared";
    $Event[1712] = "road cleared";
    $Event[1720] = "rail services irregular";
    $Event[1721] = "public transport services not operating";
    $Event[1731] = "(Q) abnormal load(s), danger";
    $Event[1732] = "(Q) wide load(s), danger";
    $Event[1733] = "(Q) long load(s), danger";
    $Event[1734] = "(Q) slow vehicle(s), danger";
    $Event[1735] = "(Q) track-laying vehicle(s), danger";
    $Event[1736] = "(Q) vehicle(s) carrying hazardous materials. Danger";
    $Event[1737] = "(Q) convoy(s), danger";
    $Event[1738] = "(Q) military convoy(s), danger";
    $Event[1739] = "(Q) overheight load(s), danger";
    $Event[1740] = "abnormal load causing slow traffic. Delays (Q)";
    $Event[1741] = "convoy causing slow traffic. Delays (Q)";
    $Event[1751] = "(Q) abnormal load(s)";
    $Event[1752] = "(Q) wide load(s)";
    $Event[1753] = "(Q) long load(s)";
    $Event[1754] = "(Q) slow vehicle(s)";
    $Event[1755] = "(Q) convoy(s)";
    $Event[1756] = "abnormal load. Delays (Q)";
    $Event[1757] = "abnormal load. Delays (Q) expected";
    $Event[1758] = "abnormal load. Long delays (Q)";
    $Event[1759] = "convoy causing delays (Q)";
    $Event[1760] = "convoy. Delays (Q) expected";
    $Event[1761] = "convoy causing long delays (Q)";
    $Event[1762] = "exceptional load warning cleared";
    $Event[1763] = "message cancelled";
    $Event[1764] = "(Q) track-laying vehicle(s)";
    $Event[1765] = "(Q) vehicle(s) carrying hazardous materials";
    $Event[1766] = "(Q) military convoy(s)";
    $Event[1767] = "(Q) abnormal load(s). No overtaking";
    $Event[1768] = "Vehicles carrying hazardous materials have to stop at next safe place!";
    $Event[1769] = "hazardous load warning cleared";
    $Event[1770] = "convoy cleared";
    $Event[1771] = "warning cleared";
    $Event[1801] = "lane control signs not working";
    $Event[1802] = "emergency telephones not working";
    $Event[1803] = "emergency telephone number not working";
    $Event[1804] = "(Q sets of) traffic lights not working";
    $Event[1805] = "(Q sets of) traffic lights working incorrectly";
    $Event[1806] = "level crossing failure";
    $Event[1807] = "(Q sets of) traffic lights not working. Stationary traffic";
    $Event[1808] = "(Q sets of) traffic lights not working. Danger of stationary traffic";
    $Event[1809] = "(Q sets of) traffic lights not working. Queuing traffic";
    $Event[1810] = "(Q sets of) traffic lights not working. Danger of queuing traffic";
    $Event[1811] = "(Q sets of) traffic lights not working. Slow traffic";
    $Event[1812] = "(Q sets of) traffic lights not working. Slow traffic expected";
    $Event[1813] = "(Q sets of) traffic lights not working. Heavy traffic";
    $Event[1814] = "(Q sets of) traffic lights not working. Heavy traffic expected";
    $Event[1815] = "(Q sets of) traffic lights not working. Traffic flowing freely";
    $Event[1816] = "(Q sets of) traffic lights not working. Traffic building up";
    $Event[1817] = "traffic lights not working. Delays (Q)";
    $Event[1818] = "traffic lights not working. Delays (Q) expected";
    $Event[1819] = "traffic lights not working. Long delays (Q)";
    $Event[1820] = "level crossing failure. Stationary traffic";
    $Event[1821] = "level crossing failure. Danger of stationary traffic";
    $Event[1822] = "level crossing failure. Queuing traffic";
    $Event[1823] = "level crossing failure. Danger of queuing traffic";
    $Event[1824] = "level crossing failure. Slow traffic";
    $Event[1825] = "level crossing failure. Slow traffic expected";
    $Event[1826] = "level crossing failure. Heavy traffic";
    $Event[1827] = "level crossing failure. Heavy traffic expected";
    $Event[1828] = "level crossing failure. Traffic flowing freely";
    $Event[1829] = "level crossing failure. Traffic building up";
    $Event[1830] = "level crossing failure. Delays (Q)";
    $Event[1831] = "level crossing failure. Delays (Q) expected";
    $Event[1832] = "level crossing failure. Long delays (Q)";
    $Event[1833] = "electronic signs repaired";
    $Event[1834] = "emergency call facilities restored";
    $Event[1835] = "traffic signals repaired";
    $Event[1836] = "level crossing now working normally";
    $Event[1837] = "message cancelled";
    $Event[1838] = "lane control signs working incorrectly";
    $Event[1839] = "lane control signs operating";
    $Event[1840] = "variable message signs not working";
    $Event[1841] = "variable message signs working incorrectly";
    $Event[1842] = "variable message signs operating";
    $Event[1843] = "(Q sets of) ramp control signals not working";
    $Event[1844] = "(Q sets of) ramp control signals working incorrectly";
    $Event[1845] = "(Q sets of) temporary traffic lights not working";
    $Event[1846] = "(Q sets of) temporary traffic lights working incorrectly";
    $Event[1847] = "traffic signal control computer not working";
    $Event[1848] = "traffic signal timings changed";
    $Event[1849] = "tunnel ventilation not working";
    $Event[1850] = "lane control signs not working. Danger";
    $Event[1851] = "temporary width limit (Q)";
    $Event[1852] = "temporary width limit lifted";
    $Event[1854] = "traffic regulations have been changed";
    $Event[1855] = "less than parking spaces available";
    $Event[1856] = "no parking information available (until Q)";
    $Event[1857] = "message cancelled";
    $Event[1861] = "temporary height limit (Q)";
    $Event[1862] = "temporary height limit lifted";
    $Event[1864] = "lane control signs working incorrectly. Danger";
    $Event[1865] = "emergency telephones out of order. Extra police patrols in operation";
    $Event[1866] = "emergency telephones out of order. In emergency, wait for police patrol";
    $Event[1867] = "(Q sets of) traffic lights not working. Danger";
    $Event[1868] = "traffic lights working incorrectly. Delays (Q)";
    $Event[1869] = "traffic lights working incorrectly. Delays (Q) expected";
    $Event[1870] = "traffic lights working incorrectly. Long delays (Q)";
    $Event[1871] = "temporary axle load limit (Q)";
    $Event[1872] = "temporary gross weight limit (Q)";
    $Event[1873] = "temporary gross weight limit lifted";
    $Event[1874] = "temporary axle weight limit lifted";
    $Event[1875] = "(Q sets of) traffic lights working incorrectly. Danger";
    $Event[1876] = "temporary traffic lights not working. Delays (Q)";
    $Event[1877] = "temporary traffic lights not working. Delays (Q) expected";
    $Event[1878] = "temporary traffic lights not working. Long delays (Q)";
    $Event[1879] = "(Q sets of) temporary traffic lights not working. Danger";
    $Event[1880] = "traffic signal control computer not working. Delays (Q)";
    $Event[1881] = "temporary length limit (Q)";
    $Event[1882] = "temporary length limit lifted";
    $Event[1883] = "message cancelled";
    $Event[1884] = "traffic signal control computer not working. Delays (Q) expected";
    $Event[1885] = "traffic signal control computer not working. Long delays (Q)";
    $Event[1886] = "normal parking restrictions lifted";
    $Event[1887] = "special parking restrictions in force";
    $Event[1888] = "10% full";
    $Event[1889] = "20% full";
    $Event[1890] = "30% full";
    $Event[1891] = "40% full";
    $Event[1892] = "50% full";
    $Event[1893] = "60% full";
    $Event[1894] = "70% full";
    $Event[1895] = "80% full";
    $Event[1896] = "90% full";
    $Event[1897] = "less than parking spaces available";
    $Event[1898] = "less than parking spaces available";
    $Event[1899] = "less than parking spaces available";
    $Event[1900] = "less than parking spaces available";
    $Event[1901] = "next departure (Q)";
    $Event[1902] = "next departure (Q) for heavy vehicles";
    $Event[1903] = "car park (Q) full";
    $Event[1904] = "all car parks (Q) full";
    $Event[1905] = "less than (Q) car parking spaces available";
    $Event[1906] = "park and ride service operating (until Q)";
    $Event[1907] = "(null event) {no event description, but location etc. given in message}";
    $Event[1908] = "switch your car radio (to Q)";
    $Event[1909] = "alarm call: important new information on this frequency follows now in norma";
    $Event[1910] = "alarm set: new information will be broadcast between these times in normal";
    $Event[1911] = "message cancelled";
    $Event[1913] = "switch your car radio (to Q)";
    $Event[1914] = "no information available (until Q)";
    $Event[1915] = "this message is for test purposes only (number Q), please ignore";
    $Event[1916] = "no information available (until Q) due to technical problems";
    $Event[1918] = "full";
    $Event[1920] = "only a few parking spaces available";
    $Event[1921] = "(Q) parking spaces available";
    $Event[1922] = "expect car park to be full";
    $Event[1923] = "expect no parking spaces available";
    $Event[1924] = "multi story car parks full";
    $Event[1925] = "no problems to report with park and ride services";
    $Event[1926] = "no parking spaces available";
    $Event[1927] = "no parking (until Q)";
    $Event[1928] = "special parking restrictions lifted";
    $Event[1929] = "urgent information will be given (at Q) on normal programme broadcasts";
    $Event[1930] = "this TMC-service is not active (until Q)";
    $Event[1931] = "detailed information will be given (at Q) on normal programme broadcasts";
    $Event[1932] = "detailed information is provided by another TMC service";
    $Event[1934] = "no park and ride information available (until Q)";
    $Event[1938] = "park and ride information service resumed";
    $Event[1940] = "additional regional information is provided by another TMC service";
    $Event[1941] = "additional local information is provided by another TMC service";
    $Event[1942] = "additional public transport information is provided by another TMC service";
    $Event[1943] = "national traffic information is provided by another TMC service";
    $Event[1944] = "this service provides major road information";
    $Event[1945] = "this service provides regional travel information";
    $Event[1946] = "this service provides local travel information";
    $Event[1947] = "no detailed regional information provided by this service";
    $Event[1948] = "no detailed local information provided by this service";
    $Event[1949] = "no cross-border information provided by this service";
    $Event[1950] = "information restricted to this area";
    $Event[1951] = "no new traffic information available (until Q)";
    $Event[1952] = "no public transport information available";
    $Event[1953] = "this TMC-service is being suspended (at Q)";
    $Event[1954] = "active TMC-service will resume (at Q)";
    $Event[1955] = "reference to audio programmes no longer valid";
    $Event[1956] = "reference to other TMC services no longer valid";
    $Event[1957] = "previous announcement about this or other TMC services no longer valid";
    $Event[1961] = "allow emergency vehicles to pass in the carpool lane";
    $Event[1962] = "carpool lane available for all vehicles";
    $Event[1963] = "police directing traffic via the carpool lane";
    $Event[1971] = "police directing traffic";
    $Event[1972] = "buslane available for all vehicles";
    $Event[1973] = "police directing traffic via the buslane";
    $Event[1974] = "allow emergency vehicles to pass";
    $Event[1977] = "allow emergency vehicles to pass in the heavy vehicle lane";
    $Event[1978] = "heavy vehicle lane available for all vehicles";
    $Event[1979] = "police directing traffic via the heavy vehicle lane";
    $Event[1982] = "buslane closed";
    $Event[2000] = "closed due to smog alert (until Q)";
    $Event[2006] = "closed for vehicles with less than three occupants {not valid for lorries}";
    $Event[2007] = "closed for vehicles with only one occupant {not valid for lorries}";
    $Event[2013] = "service area busy";
    $Event[2021] = "service not operating, substitute service available";
    $Event[2022] = "public transport strike";
    $Event[2028] = "message cancelled";
    $Event[2029] = "message cancelled";
    $Event[2030] = "message cancelled";
    $Event[2033] = "message cancelled";
    $Event[2034] = "message cancelled";
    $Event[2035] = "message cancelled";
    $Event[2038] = "message cancelled";
    $Event[2039] = "message cancelled";
    $Event[2040] = "message cancelled";
    $Event[2047] = "(null message) {completely silent message, see protocol, sect. 3.5.4}";

    # DI table

    $di{'011'} = "Mono / PS char 7,8";
    $di{'111'} = "Stereo / PS char 7,8";
    $di{'010'} = "Not Artificial Head / PS char 5,6";
    $di{'110'} = "Artificial Head / PS char 5,6";
    $di{'001'} = "Not compressed / PS char 3,4";
    $di{'101'} = "Compressed / PS char 3,4";
    $di{'000'} = "Static PTY / PS char 1,2";
    $di{'100'} = "Dynamic Switch / PS char 1,2";

    # PTY codes

    $pty{'00000'} = "0  - None";              $pty{'00001'} = "1  - News";
    $pty{'00010'} = "2  - Current Affairs";   $pty{'00011'} = "3  - Information";
    $pty{'00100'} = "4  - Sport";             $pty{'00101'} = "5  - Education";
    $pty{'00110'} = "6  - Drama";             $pty{'00111'} = "7  - Culture";
    $pty{'01000'} = "8  - Science";           $pty{'01001'} = "9  - Varied Speech";
    $pty{'01010'} = "10 - Pop Music";         $pty{'01011'} = "11 - Rock Music";
    $pty{'01100'} = "12 - Easy Listening";    $pty{'01101'} = "13 - Light Classical";
    $pty{'01110'} = "14 - Serious Classical"; $pty{'01111'} = "15 - Other Music";
    $pty{'10000'} = "16 - Weather & Metr";    $pty{'10001'} = "17 - Finance";
    $pty{'10010'} = "18 - Children's Progs";  $pty{'10011'} = "19 - Social Affairs";
    $pty{'10100'} = "20 - Religion";          $pty{'10101'} = "21 - Phone In";
    $pty{'10110'} = "22 - Travel & Touring";  $pty{'10111'} = "23 - Leisure";
    $pty{'11000'} = "24 - Jazz Music";        $pty{'11001'} = "25 - Country Music";
    $pty{'11010'} = "26 - National Music";    $pty{'11011'} = "27 - Oldies Music";
    $pty{'11100'} = "28 - Folk Music";        $pty{'11101'} = "29 - Documentary";
    $pty{'11110'} = "30 - Alarm Test";        $pty{'11111'} = "31 - Alarm - Alarm !";

    # Group codes

    $grp{'00000'} = "0A  - Tuning";       $grp{'00001'} = "0B  - Tuning";
    $grp{'00010'} = "1A  - Item Number";  $grp{'00011'} = "1B  - Item Number";
    $grp{'00100'} = "2A  - RadioText";    $grp{'00101'} = "2B  - RadioText";
    $grp{'00110'} = "3A  - ODA ID";       $grp{'00111'} = "3B  - Open Data";
    $grp{'01000'} = "4A  - Clock/Time";   $grp{'01001'} = "4B  - Open Data";
    $grp{'01010'} = "5A  - TDC / ODA";    $grp{'01011'} = "5B  - TDC / ODA";
    $grp{'01100'} = "6A  - Open Data";    $grp{'01101'} = "6B  - Open Data";
    $grp{'01110'} = "7A  - Radio Paging"; $grp{'01111'} = "7B  - Open Data";
    $grp{'10000'} = "8A  - TMC";          $grp{'10001'} = "8B  - Open Data";
    $grp{'10010'} = "9A  - Emerg / ODA";  $grp{'10011'} = "9B  - Open Data" ;
    $grp{'10100'} = "10A - PTN";          $grp{'10101'} = "10B - Open Data";
    $grp{'10110'} = "11A - Open Data";    $grp{'10111'} = "11B - Open Data";
    $grp{'11000'} = "12A - Open Data";    $grp{'11001'} = "12B - Open Data";
    $grp{'11010'} = "13A - Radio Paging"; $grp{'11011'} = "13B - Open Data";
    $grp{'11100'} = "14A - Net Info";     $grp{'11101'} = "14B - Net Info";
    $grp{'11110'} = "15A - RDBS";         $grp{'11111'} = "15B - Switching Info";

    # Multi Group

    $label{'0'}{'size'}  = "3";  $label{'0'}{'msg'}  = "Duration";
    $label{'1'}{'size'}  = "3";  $label{'1'}{'msg'}  = "Control code";
    $label{'2'}{'size'}  = "5";  $label{'2'}{'msg'}  = "Length";
    $label{'3'}{'size'}  = "5";  $label{'3'}{'msg'}  = "Speed limit";
    $label{'4'}{'size'}  = "5";  $label{'4'}{'msg'}  = "Quantifier";
    $label{'5'}{'size'}  = "8";  $label{'5'}{'msg'}  = "Quantifier";
    $label{'6'}{'size'}  = "8";  $label{'6'}{'msg'}  = "Info code";
    $label{'7'}{'size'}  = "8";  $label{'7'}{'msg'}  = "Start time";
    $label{'8'}{'size'}  = "8";  $label{'8'}{'msg'}  = "Stop time";
    $label{'9'}{'size'}  = "11"; $label{'9'}{'msg'}  = "Event";
    $label{'10'}{'size'} = "16"; $label{'10'}{'msg'} = "Diversion";
    $label{'11'}{'size'} = "16"; $label{'11'}{'msg'} = "Location";
    $label{'12'}{'size'} = "16"; $label{'12'}{'msg'} = "unknown";
    $label{'13'}{'size'} = "16"; $label{'13'}{'msg'} = "Location";
    $label{'14'}{'size'} = "0";  $label{'14'}{'msg'} = "NOP";
    $label{'15'}{'size'} = "0";  $label{'14'}{'msg'} = "unknown";
}

sub usage {
   die "Simple RDS-TMC Decoder 0.2     || http://dev.inversepath.com/rds
Copyright 2007 Andrea Barisani || <andrea\@inversepath.com>
Usage: $0 [-h|-H|-P|-t] [-d <location db path>] [-p <PI number>] <input file>
   -t display only tmc packets
   -H HTML output (outputs to /tmp/rds-<random>/rds-*.html)
   -p PI number
   -P PI search
   -d location db path
   -h this help

Note: -d option expects a DAT Location Table code according to TMCF-LT-EF-MFF-v06
      standard (2005/05/11)\n\n";
}
