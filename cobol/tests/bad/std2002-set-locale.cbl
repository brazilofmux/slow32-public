identification division.
program-id. p24.
*> SET LOCALE (2023 14.9.39 format 11, docs/plans/locale.md step 1): the
*> categories are the words of 8.2, spelled with underscores -- LC-ALL is not
*> one. Before step 1 this statement was refused as not implemented.
data division.
working-storage section.
procedure division.
    set locale lc-all to user-default
    goback.
