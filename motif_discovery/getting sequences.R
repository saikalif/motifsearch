#you need a package that can read fasta files. Biostrings is a good one. 
library(Biostrings)

#load all the fasta files so that the entire NCBIM37 genome is uploaded. (genomes from ftp://ftp.ensembl.org/pub/release-67/fasta/mus_musculus/dna/)
chr1<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.1.fa")
chr2<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.2.fa")
chr3<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.3.fa")
chr4<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.4.fa")
chr5<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.5.fa")
chr6<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.6.fa")
chr7<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.7.fa")
chr8<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.8.fa")
chr9<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.9.fa")
chr10<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.10.fa")
chr11<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.11.fa")
chr12<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.12.fa")
chr13<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.13.fa")
chr14<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.14.fa")
chr15<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.15.fa")
chr16<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.16.fa")
chr17<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.17.fa")
chr18<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.18.fa")
chr19<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.19.fa")
chrMT<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.MT.fa")
chrX<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.X.fa")
chrY<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.Y.fa")
chrnon<-readDNAStringSet("chromosomes/Mus_musculus.NCBIM37.67.dna.nonchromosomal.fa")

chr1<-readDNAStringSet(c("chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.1.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.2.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.3.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.4.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.5.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.6.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.7.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.8.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.9.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.10.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.11.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.12.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.13.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.14.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.15.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.16.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.17.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.18.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.19.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.MT.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.X.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.chromosome.Y.fa",
                         "chromosomes/Mus_musculus.NCBIM37.67.dna.nonchromosomal.fa")
#put all the chromosomes into an indexible list. This list is named, so calling genome$chr1 should return the sequence of the first chromosome.
genome<-list(chr1=chr1,
             chr2=chr2,
             chr3=chr3,
             chr4=chr4,
             chr5=chr5,
             chr6=chr6,
             chr7=chr7,
             chr8=chr8,
             chr9=chr9,
             chr10=chr10,
             chr11=chr11,
             chr12=chr12,
             chr13=chr13,
             chr14=chr14,
             chr15=chr15,
             chr16=chr16,
             chr17=chr17,
             chr18=chr18,
             chr19=chr19,
             chrX=chrX,
             chrY=chrY,
             chrMT=chrMT,
             chrnon=chrnon)

sequencenametrial<-function(thing){
  seq_name<-c()
  for (i in 1:length(thing)){
    seq_name<-c(seq_name,names(thing[i]))
  }
  return(seq_name)
}

sequencelist<-function(thing){
  sequences<-c()
  for (i in 1:length(thing)){
    sequences<-c(sequences,paste(thing[i]))
  }
}




#example to test that this works
genome$chr1

#test to get a subsequence
subseq(genome$chr2,start=10700000, end=10700030)

#it works!!
#now we need to make it more complicated 