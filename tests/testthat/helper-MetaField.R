# data for tests of MetaField-class
#
# Author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 29.05.2020
###############################################################################

if ( TRUE )
{
res <- list()
res$a <- MetaField$new(name = 'title', data = LanguageString$new(de = 'Titel'))
res$b <- MetaField$new(name = 'source_name', data = LanguageString$new(de = 'Bildungsplanung Kanton Zürich'))
res$c <- MetaField$new(name = 'source_url', data = 'https://bi.zh.ch/bildungsplanung')
res$d <- MetaField$new(name = 'issued', data = lubridate::ymd_hm('2020-05-25 2023'))
res$e <- MetaField$new(name = 'modified', data = Sys.time())
res$f <- MetaField$new(name = 'units', data = list('all' = LanguageString$new(en = 'Count')))
res$g <- MetaField$new(name = 'aggregate', data = list(m1 = 'mean', m2 = 'sum'))
res$h <- MetaField$new(name = 'dim_order', data = c('m1', 'm3', 'm2'))
res$i <- MetaField$new(name = 'hierarchy', data = list(ma = 'm1', mb = list('m2', mc = 'm3')))
res$j <- MetaField$new(name = 'labels', data = list(dimnames = list(m1 = LanguageString$new(de = 'erstes Merkmal'),
                                                                    m2 = LanguageString$new(en = 'second feature')),
                                                    m1 = list(v1 = LanguageString$new(de = 'erster Wert des ersten Merkmals'))))
res$k <- MetaField$new(name = 'description', data = list(all = LanguageString$new(de = 'Beschreibung des ganzen Datensatzes'),
                                                         dim = list(m1 = LanguageString$new(en = 'Description of the first feature'),
                                                                    m2 = LanguageString$new(fr = 'Description de la deuxieme caracteristique')),
                                                         m1 = list(v1 = LanguageString$new(de = c('Beschreibung eines Wertes von m1', 'Hier muss man besonders aufpassen')))))

}
