---
title: "{{ replace .Name "-" " " | title }}"
date: {{ .Date }}
draft: true
description: ""
tags: ["example-tag"]
# Note: No categories field - causes raw text leak in Hextra theme
# Note: No author field - uses site-level config (params.author in hugo.yaml)
---

[Content here]
