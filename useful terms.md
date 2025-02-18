# Useful terms

## Regex
-   `(?i)` - ignores uppercase. Thus often used to begin an expression, for example `(?i)england`.
-   `\\` - used when setting regex in R as not to be confused with the escape character `\`. The bar(s) have no meaning unless accompanied by other characters to signify a modifier, as you'll see below.
-   If you wish to test regex outside R, for example in [regex101.com](https://regex101.com/), then use `cat()`. For example, `cat('\\bpatient')` will return `\bpatient` which you can test in regex 101 to check if it behaves as expected.
-   `\\b` - a word boundary, which can be placed at the beginning and/or end of a term to restrict generality. For example, `\\bpatient` would capture mentions of "patient" and "patients" but ignore mentions of "impatient", whilst `\\bpatient\\b` will additionally ignore mentions of "patients".
-   `|`- or. It provides the option of looking for variations anywhere in a string. For instance, `theatre|theater` will look for both spellings of the word "theatre". This expression could be shortened as `theat(re|er)` for a more succinct approach. We often use `(|)` to account for sounds that could lead to misspellings, for example `sul(f|ph)ate`.
-   `?` - optional presence. It can be used to omit looking for a character in a string. Placed to the right of the character we wish to ignore. For example, `oph?th?al?mology` will match the correct spelling of "ophthalmology" and any misspellings that omitted either of the letters that had a question mark next to them (e.g., "optamology", "optalmology", "ophtamology")
-   `(?:\\W|)` - optional non-word character including. Useful for acronyms (e.g., NHS) and compound words that can be spelled with a hyphen (e.g., part-time). Therefore placed between characters. For example, intravenous is often abbreviated to IV or I.V, so one could use `I(?:\\W|)V`. That expression would also capture "I-V", as the non-word character could be anything, including a space. In practice, we would use a more sophisticated expression to restrict generality and potentially ignore capitalisation `(?i)\\bI(?:\\W|)V\\b`.
-   You can omit certain terms from your search, in the below example the term 'Frida' is open ended to capture options such as 'Frida Baby', but we don't want 'Friday' returned `(?i)\\bfrida(?!y)`
-   Below is how you search for a range of numbers, for example "0 - 40 weeks" pregnant: `\\b([0-9]|[1-3][0-9]|40)\\sweeks?\\b`
-   Below is regex for when you want to find a word within a certain number of other words, with the option for a reverse of the pattern as well:
-   `\\b(?:word1\\W+(?:\\w+\\W+){0,5}?word2|word2\\W+(?:\\w+\\W+){0,5}?word1)\\b`
-   You might come across instances where a special character is part of the string you're after, but you'll struggle to set the correct regex expression because that character has a meaning in regex code. Use `fixed()` in such instances. For example `str_detect('65+', '+')` will error out but `str_detect('65+', fixed('+'))` will not.
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
