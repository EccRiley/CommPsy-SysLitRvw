---
title: "Reading List Search Results"
---

# \Large{Search Files Using \texttt{"grep"}}

-----

```{bash }
cd Dropbox/ReadingList
ls *.pdf
```

```{bash }
cat > searchResults.md
'
---
title: "Reading List Search Results"
---

-----

# Community Psychology

## Search = "`grep -ic 'community psychology' *.pdf`"

```

'
## [CTRL-D] ##
```

```{bash }
grep -ic 'community psychology' *.pdf >> searchResults.md
cat >> searchResults.md
'
```

-----

'
## [CTRL-D] ##

```

```{bash }
cat >> searchResults.md
'

## Search = "`grep -ic 'action research' *.pdf`"

```

'

## [CTRL-D] ##

```

```{bash }
grep -ic 'action research' *.pdf >> searchResults.md
cat >> searchResults.md
'
```

-----

'
## [CTRL-D] ##

```

```{bash }
cat >> searchResults.md
'

## Search = "`grep -ic 'participatory' *.pdf`"

```

'

## [CTRL-D] ##

```

```{bash }
grep -ic 'participatory' *.pdf >> searchResults.md
cat >> searchResults.md
'
```

-----

'
## [CTRL-D] ##

```


```{bash }
cat >> searchResults.md
'

## Search = "`grep -ic 'quantitative' *.pdf`"

```

'

## [CTRL-D] ##

```

```{bash }
grep -ic 'quantitative' *.pdf >> searchResults.md
cat >> searchResults.md
'
```

-----

'
## [CTRL-D] ##

```

```{bash }
cat >> searchResults.md
'

## Search = "`grep -ic 'qualitative' *.pdf`"

```
'

## [CTRL-D] ##

```

```{bash }
grep -ic 'qualitative' *.pdf >> searchResults.md
cat >> searchResults.md
'
```

-----

'
## [CTRL-D] ##

```
```{bash }
cat >> searchResults.md
'

## Search = "`grep -ic 'mixed methods' *.pdf`"

```
'

## [CTRL-D] ##

```

```{bash }
grep -ic 'mixed methods' *.pdf >> searchResults.md
cat >> searchResults.md
'
```

-----

'
## [CTRL-D] ##

```
