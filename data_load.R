#Get the data from the source for the dates
dfModern=generate_df(ModernRawFile)

dfPioneer=generate_df(PioneerRawFile)

dfPauper=generate_df(PauperRawFile)

#Get the card data
cardDataSub=getCardData(CardFile)
