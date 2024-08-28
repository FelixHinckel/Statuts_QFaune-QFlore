# Statuts_QFaune-QFlore

Deux scripts (Faune et Flore) Pour réaliser sur R les listes d'espèces et joindre les statuts nationaux et régionaux. 

Le script récupère le TaxRef, la BDC, et quelques données patch, non disponibles dans la BDC. Ces fichiers sont stockées dans data.zip, étant trop volumineux pour être gérés directement pa GitHub.

Deux types de sorties : un GPKG à intégrer dans le projet Qgis, et des CSV (Liste et Region) qui peuvent être lues hors SIG.

Les scripts sont conçus pour être réutilisables à chaque occurence TaxRef/BDC. Attention cependant aux données Patch, qui ne sont pas forcément à conserver d'une année sur l'autre. 

Les scripts sont pensés pour être utilisable dans QFaune et QFlore, et rester aussi léger que possible. Toute idée pour améliorer l'utilisation ou réduire le poids du GPKG est la bienvenue! Ces scripts ont aussi vocation à intégrer d'autres données non présentes dans la BDC (Espces humides et EEE par exemple). N'hésitez pas à proposer des améliorations ou des données supplémentaires utiles.
