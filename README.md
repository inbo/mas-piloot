# MAS-pilootproject

Pilootproject voor de monitoring van vogels en sommige zoogdieren in het agrarisch gebied volgens de MAS methodiek (MAS = meetnet agrarische soorten).

## Doel van de repo

Deze repository bevat R code voor:

-   opstellen van steekproefkader voor MAS tellingen
-   steekproeftrekking voor elk van de pilootgebieden
-   verkennende analyses van historische MAS tellingen
-   Densiteitsschattingen voor de soorten aan de hand van distance sampling modellen
-   Analyses voor het bepalen van de factoren die meest verklarend zijn voor verschillen in toestand of trends in soortenaantallen

## Repo structuur


```
.
+-- data
+-- media
\-- src
    +-- markdown
    +-- R
    +-- targets
+-- renv
+-- .gitignore
+-- LICENSE
+-- mas-piloot.Rproj
+-- README.md
+-- renv.lock

```


De meeste data onder de map `./data/` dienen te worden gedownload met behulp van de functies onder de map `./R/`.

Wanneer al deze data gedownload worden, zal de repo er lokaal ongeveer zo uitzien (de grote bestanden vallen niet onder het `git` versiebeheer):

```
.
├── data
│   ├── abv
│   ├── bo_vlm
│   ├── c-mon
│   ├── dem
│   ├── groenkaart
│   ├── landbouwgebruikspercelen
│   ├── landbouwstreken
│   ├── landgebruik
│   ├── lucas_harmonised
│   ├── mas
│   ├── natuurbeheerplannen
│   ├── processed
│   ├── sbp_hamster_2015
│   ├── SOVON
│   └── vmm
```



