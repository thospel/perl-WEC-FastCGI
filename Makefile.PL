#!/usr/bin/perl -w
# $Id: Makefile.PL 2693 2008-01-02 09:54:19Z hospelt $
use 5.008_001;
use ExtUtils::MakeMaker;
our $VERSION = '1.005';

WriteMakefile
    (NAME		=> 'WEC::FastCGI',
     VERSION_FROM	=> 'FastCGI.pm', # finds $VERSION
     PREREQ_PM		=> { 
         WEC			=> 0.01,
         "Exporter::Tidy"	=> 0.05,
         "Test::More"		=> 0.11,	# For the tests only
     },
     ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (AUTHOR     => 'Ton Hospel <WEC-FastCGI@ton.iguana.be>') : ()),
     $^O eq "MSWin32" ? (
         PM_FILTER	=> '$(PERL) -p -e1',
     ) : (),
     clean		=> {
         FILES => '$(DISTNAME).ppd ppm cover_db',
     },
);

# START MY
# END MY
