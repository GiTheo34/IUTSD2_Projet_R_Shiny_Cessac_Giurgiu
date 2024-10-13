Projet d'Analyse des Adresses et Performances Énergétiques dans l'Hérault
Ce dépôt contient une application Shiny interactive pour l’analyse de données géographiques et de performances énergétiques (DPE) dans le département de l’Hérault. L'application permet une exploration visuelle des données, le téléchargement de rapports en Markdown, et inclut plusieurs fonctionnalités de personnalisation.

Contenu du Dépôt
1. Fichiers CSV
adresses-34.csv : Ce fichier contient les adresses géographiques utilisées dans l'application, incluant les codes postaux et autres informations de localisation.
DPE_Herault.csv : Fichier de données avec des informations sur les étiquettes DPE, utilisées pour analyser et afficher les performances énergétiques par adresse.
2. Documentation
Documentation technique : Ce document explique en détail la structure du code, les dépendances utilisées, et les étapes d’installation pour les développeurs souhaitant contribuer ou déployer l'application.
Documentation fonctionnelle : Une vue d'ensemble de l'application et de ses fonctionnalités principales pour les utilisateurs finaux. Elle présente les fonctionnalités clés et les options de navigation.
3. Dossier de l’Application Shiny (DPE_Herault/)
packages.R : Script listant les packages requis pour l'application. Il installe et charge automatiquement les packages nécessaires.
server.R : Code serveur de l'application, contenant la logique principale de traitement des données, l'interactivité, et les règles de gestion de l’interface.
ui.R : Code de l'interface utilisateur de l'application, gérant la mise en forme et la disposition des éléments visuels.
DPE_Herault.csv : Copie locale du fichier CSV pour le DPE.
4. Markdown Téléchargeable
Bon Markdown : Rapport Markdown généré par l’application et téléchargeable directement depuis le 2ème onglet. Il présente une analyse détaillée de l’ensemble des données DPE.
5. Dossier www
style.css : Feuille de style personnalisée pour l’application, définissant les éléments visuels tels que les couleurs, polices, et dispositions.
Images utilisées : Ressources visuelles intégrées dans l'application pour améliorer la présentation et l'interaction utilisateur.
