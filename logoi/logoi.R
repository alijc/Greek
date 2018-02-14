# logoi (λογοι)
# Inspired by Whitaker's Words, but for ancient Greek rather than Latin.
# Written, as an exercise in functional programming, in R.

genders <- c('masc', 'fem', 'neut')
gsigils <- c(' boy', ' girl', ' thing')
cases 	<- c('nom', 'acc', 'gen', 'dat')
csigils <- c('', '>', 'of ', 'for ')
numbers <- c('sg', 'pl') # save room for dual
nsigils <- c('', 's')

the.declined <- array( 
c(
	'ὁ',   'ἡ', 'τό', 
	'τόν', 'τήν', 'τό',
	'τοῦ', 'τῆς', 'τοῦ',
	'τῷ',  'τῇ', 'τῷ', 
	'οἱ',   'αἱ', 'τά', 
	'τούς', 'τάς', 'τά',
	'τῶν', 'τῶν', 'τῶν',
	'τοῖς', 'ταῖς', 'τοῖς'
	),
    dim=c(3,4,2),
    dimnames = list(genders, cases, numbers)
)

adj.endings <- array( 
    c(

	'ός',   'ή',   'όν',
	'όν',   'ήν',  'όν',   
	'οῦ',   'ῆς',  'οῦ',
	'ῷ',    'ῇ',   'ῷ',
	'οί',   'αί',   'ά',
	'ούς',  'άς',   'ά',
	'ῶν',   'ῶν',  'ῶν',
	'οῖς',   'αῖς',  'οῖς'
	),
    dim=c(3,4,2),
    dimnames = list(genders, cases, numbers)
)

#
# adj - decline an adjective, given its stem, endings and meaning
#
# Note, for our purposes, the definite article is treated as an adjective
# with a null stem.

adj <- function(stem,endings,eng) {

    for (n in 1:length(numbers)) {
	for (c in 1:length(cases)) {
	    for (g in 1:length(genders)) {
		cat ( paste(stem, endings[g,c,n], sep=""), "\t-", 
		    "adj", genders[g], cases[c], numbers[n], "\t-",
		    paste (csigils[c], eng, gsigils[g], nsigils[n], sep=""),
		    "\n" )
	    }
	}
    }
}


#adj("", the.declined, "the" ) 
adj("σοφ", adj.endings, "wise")
adj("μικρ", adj.endings, "small")
