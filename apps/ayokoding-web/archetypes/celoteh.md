---
title: "{{ replace .Name "-" " " | title }}"
date: {{ .Date }}
draft: true
description: ""
tags: ["example-tag"]
# Note: No categories field - causes raw text leak in Hextra theme
# Note: author field is OPTIONAL - only add when content has guest contributor
# Default: Uses site-level config (params.author: "Wahidyan Kresna Fridayoka")
---

بِسْــــــــــــــــــمِ اللهِ الرَّحْمَنِ الرَّحِيْمِ

Dengan nama Allah Yang Maha Pengasih, Maha Penyayang.

{{ dateFormat "2 January 2006" .Date }}

[Your personal essay/rant content here]
