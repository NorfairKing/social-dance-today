module Salsa.Party.Importers where

import Salsa.Party.Importer

allImporters :: [Importer]
allImporters =
  [ salsaFauraxFrImporter,
    eventsInfoImporter,
    golatindanceComImporter,
    danceplaceComImporter,
    mapdanceComImporter,
    salsachicagoComImporter,
    dancefloorfinderComImporter,
    salsaBeImporter,
    latinworldNlImporter,
    tanzagendaChImporter,
    stayHappeningComImporter,
    londonSalsaEventsComImporter,
    salsaLoversBeImporter,
    tanzeventsChImporter,
    danceusOrgImporter,
    mueveteChImporter,
    goanddanceComImporter
  ]
