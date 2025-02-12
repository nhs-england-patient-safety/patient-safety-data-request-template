# Useful terms

## Regex

-   `(?:\\W|)` - optional non word character including - useful for acronyms
-   Below is regex for when you want to find a word within a certain number of other words, with the option for a reverse of the pattern as well:
-   `\\b(?:word1\\W+(?:\\w+\\W+){0,5}?word2|word2\\W+(?:\\w+\\W+){0,5}?word1)\\b`
-   Below is how you search for a range of numbers, for example "0 - 40 weeks" pregnant:
-   `\\b([0-9]|[1-3][0-9]|40)\\sweeks?\\b`
-   You can omit certain terms from your search, in the below example the term 'Frida' is open ended to capture options such as 'Frida Baby', but we don't want 'Friday' returned:
-   `(?i)\\bfrida(?!y)`

## Categorical terms

### LFPSE

-   LFPSE numerical columns are strings i.e. `expr(CL001 =="4")` instead of `expr(CL001 == 4)`.

-   When searching a multi-select categorical column in LFPSE, grep and stringr functions won't work.

    -   `expr((' ' + A003 + ' ') %LIKE% '% 3 %')`

### NRLS

-   Filtering for England in NRLS:

    -   `expr(IN04 == 1 | IN04 %in% c(97, 99) & !is.na(RP07) | is.na(IN04) & !is.na(RP07))`
