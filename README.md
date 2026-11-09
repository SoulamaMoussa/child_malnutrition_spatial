# Malnutrition infantile à Nouna: dépendance spatiale et facteurs associés

Étude de la répartition géographique de la malnutrition chez les enfants de moins de 5 ans dans l'observatoire de Nouna, avec l'**indice de Moran** et une **régression logistique ordinale**.

## Contexte et problématique

Au Burkina Faso, près d'un enfant de moins de 5 ans sur quatre souffre d'un retard de croissance (23 % en 2021). Derrière cette moyenne, les écarts sont forts : jusqu'à 37 % dans la région du Sahel, contre 23 % au niveau national. La crise sécuritaire, les déplacements de population et le changement climatique aggravent la situation, car les ménages ruraux dépendent presque entièrement de la terre pour nourrir leurs enfants.

Imaginons un district sanitaire ou une ONG de nutrition qui dispose de moyens limités et doit décider **dans quels villages envoyer en priorité ses équipes**. Il lui faut savoir où la malnutrition se concentre, et pourquoi.

**Problématique :** la malnutrition infantile est-elle répartie au hasard dans l'observatoire de Nouna ? Où se trouvent les foyers à risque (*hotspots*), et quels facteurs individuels, familiaux et climatiques y sont associés ?

## Objectifs

- Décrire la répartition spatiale de la malnutrition infantile.
- Cartographier les *hotspots* avec l'indice de Moran.
- Identifier les facteurs associés à l'état nutritionnel.

## Données et méthode

- **Type d'étude :** transversale, descriptive à visée analytique.
- **Collecte :** du 1er juillet au 30 août 2022, dans les ménages de l'observatoire de Nouna (SSDS-N).
- **Échantillon :** 1 999 enfants de moins de 5 ans et leurs mères.
- **Données :** mesures anthropométriques, caractéristiques sociodémographiques et économiques, et données climatiques issues de cinq stations météorologiques.
- **Variable à expliquer :** état nutritionnel en trois niveaux, construit à partir des z-scores OMS (taille pour l'âge, poids pour l'âge, poids pour la taille) : *normal*, *moyen* (une seule forme de malnutrition) et *sévère* (plusieurs formes).
- **Analyse spatiale :** indice de Moran global (la répartition est-elle aléatoire ?) et local (où sont les foyers ?).
- **Facteurs associés :** régression logistique ordinale.

## Résultats

- **46 % des enfants sont malnutris** : 28 % présentent une seule forme de malnutrition et 18 % en cumulent plusieurs.
- **La répartition n'est pas aléatoire.** Des foyers significatifs apparaissent dans plusieurs villages du nord de l'observatoire : Soin, Séré, Dina, Sériba et Cissé.
- **Le contexte climatique compte.** Les groupes de villages définis par leur climat sont significativement associés au niveau de malnutrition (p < 0,05).
- **Facteurs associés :**
  - individuels : sexe masculin, sous-alimentation, écoulement nasal pendant 4 semaines ;
  - maternels : niveau d'études de la mère ;
  - du ménage : indice de richesse, commune de résidence.

## Conclusion

**Ce que les données racontent**

1. **La situation.** Presque un enfant sur deux (46 %) est touché par au moins une forme de malnutrition, et près d'un sur cinq en cumule plusieurs.
2. **La rupture.** Cette malnutrition n'est pas diffuse : elle se concentre dans des villages précis du nord de l'observatoire.
3. **L'explication.** La localisation et le contexte climatique pèsent fortement, en plus de facteurs familiaux et individuels comme la richesse du ménage et le niveau d'études de la mère.

**Ce que cela permet de faire**

- Cibler **en priorité les villages à risque élevé** pour les interventions nutritionnelles.
- Tenir compte des **profils climatiques** et des **facteurs socio-économiques locaux** dans leur conception.
- **Intégrer l'analyse spatiale** aux programmes de nutrition pour mieux répartir des ressources limitées.

## Limites

- L'étude est transversale : elle met en évidence des **associations**, pas des relations de cause à effet.
- Les données portent sur une seule période (été 2022) et un seul observatoire : les conclusions ne s'étendent pas telles quelles à tout le pays.

## Outils

[À compléter : logiciels et packages utilisés, par exemple R, packages de statistique spatiale, QGIS.]

## Données et confidentialité

Les données concernent la santé d'enfants : **elles ne sont pas publiées dans ce dépôt**. Seuls le code et la description de la démarche sont disponibles.


