#!/usr/bin/perl -w
if ($#ARGV != 0) {
    print "ms2txt.pl <*.ms file>\n";
} else {
    print "Program started...\n";
}

$filename = $ARGV[0];

open(FILE, "<$filename") or die "Could not open $filename: $!\n";
if ($filename =~ m/([\w\d_\-]*).ms/) {
    $outname = $1 . ".txt";
    print "Output file: $outname\n";
}
open(OUTPUT, ">$outname") or die "Could not write to $outname: $!\n";

while (<FILE>) {
    $string = $_;
    if ($string =~ s/@Comment[{|\(][\S\s]*[}|\)]?$//i) {
	print "Found Comment: ", $string, "\n";
    } 
    if ($string =~ s/@Make\([\S\s]*\)?$//i) {
	print "Found Make: ", $string, "\n";
    }
    if ($string =~ s/@Define\([\S\s]*\)?$//i) {
	print "Found Define: ", $string, "\n";
    }
    if ($string =~ s/@TCT\([\S\s]*\)?$//i) {
	print "Found TCT: ", $string, "\n";
    }
    if ($string =~ s/@String\([\S\s]*\)?$//i) {
	print "Found String: ", $string, "\n";
    }
    if ($string =~ s/@Case\([\S\s]*\)?$/LISPMACHINE/i) {
	print "Found Case: ", $string, "\n";
    }
    if ($string =~ s/@Form\([\S\s]*\)?$//i) {
	print "Found Form: ", $string, "\n";
    }
    if ($string =~ s/@char\([\S\s]*\)?$//i) {
	print "Found char: ", $string, "\n";
    }
    if ($string =~ s/@PageFooting\([\S\s]*\)?$//i) {
	print "Found PageFooting: ", $string, "\n";
    }
    if ($string =~ s/@Blankspace\([\S\s]*\)?$/\n/i) {
	print "Found Blankspace: ", $string, "\n";
    }
    if ($string =~ s/@MajorHeading[{|\(]([\S\s]*)[}|\)]?$/$1/i) {
	print "Found MajorHeading: ", $string, "\n";
    }
    if ($string =~ s/@Center[{|\(]([\S\s]*)[}|\)]?$/$1/i) {
	print "Found Center: ", $string, "\n";
    }
    if ($string =~ s/@Value\([\S\s]*\)?$//i) {
	print "Found Value: ", $string, "\n";
    }
    if ($string =~ s/@NewPage//i) {
	print "Found NewPage\n";
    }
    if ($string =~ s/@Begin\([\S\s]*\)?$//i) {
	print "Found Begin: ", $string, "\n";
    }
    if ($string =~ s/@End\([\S\s]*\)?$//i) {
	print "Found End: ", $string, "\n";
    }
    if ($string =~ s/@Set\([\S\s]*\)?$//i) {
	print "Found Set: ", $string, "\n";
    }
    if ($string =~ s/@Pageheading\(([\S\s]*)\)?$/$1/i) {
	print "Found Pageheading: ", $string, "\n";
    }
    if ($string =~ s/@Value\([\S\s]*\)?$//i) {
	print "Found Value: ", $string, "\n";
    }
    if ($string =~ s/@Escape\([\S\s]*\)?$//i) {
	print "Found Escape: ", $string, "\n";
    }
    if ($string =~ s/@PrefaceSection\(([\S\s]*)\)?$/$1/i) {
	print "Found PrefaceSection: ", $string, "\n";
    }
    if ($string =~ s/@i<([\S\s]*)>/$1/i) {
	print "Substitution i: ", $string, "\n";
    }
    if ($string =~ s/@Index([\S\s]*)//i) {
	print "Substitution Index: ", $string, "\n";
    }
    if ($string =~ s/@lt<([\S\s]*)>/$1/i) {
	print "Substitution lt: ", $string, "\n";
    }
    if ($string =~ s/@~//i) {
	print "Substitution ~: ", $string, "\n";
    }
    if ($string =~ s/@Tag\([\S\s]*\)?$//i) {
	print "Found Tag: ", $string, "\n";
    }
    if ($string =~ s/@SubSection\(([\S\s]*)\)?$/$1/i) {
	print "Substitute SubSection: ", $string, "\n";
    }
    if ($string =~ s/@Section\(([\S\s]*)\)?$/$1/i) {
	print "Substitute Section: ", $string, "\n";
    }
    if ($string neq "") {
	print OUTPUT $string, "\n";
    }
     
}

close(FILE);
close(OUTPUT);
