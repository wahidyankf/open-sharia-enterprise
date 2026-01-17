---
title: "I Migrated AyoKoding Com from Gitbook to Notion and Super"
weight: 10050
date: 2023-06-01T07:20:00+07:00
draft: false
---

**Similar article:** [Migrasi ayokoding.com dari Gitbook ke Notion dan Super](/id/celoteh/2023/06/migrasi-ayokoding-com-dari-gitbook-ke-notion-dan-super)

بِسْــــــــــــــــــمِ اللهِ الرَّحْمَنِ الرَّحِيْمِ

In the name of Allah, The Most Gracious and The Most Merciful

June 1, 2023

As a self-proclaimed content creator, I have always sought ways to make my content creation process more efficient. I also wanted to make it easier to share needed material with my engineering team at work, establish my branding, and take advantage of the advent of AI to write efficiently. I had a YouTube channel called AyoKoding, and it took almost 10 hours to produce one piece of content, which I didn't have much time for lately (i.e., I have 2 cute little kids at home).

## Gitbook made me up and running

After some research, I wrote my content ([ayokoding.com](http://ayokoding.com/)) at [Gitbook.com](http://gitbook.com/). It was free, supported a custom domain, and got me up and running. The SEO was primarily working, and the server-side rendering and beautiful URL were impressive. However, the platform could have been more fluid in the writing experience, and I can't copy the article back to the chatGPT prompt without losing its markdown formatting. The template/layout was also so-so, and it didn't allow me to inject a custom script/CSS/HTML. I wanted to do some automation to my content, such as automatically translating the English section to Bahasa.

## I want to migrate the editing to Notion

I already subscribed to Notion, and the editing experience was superb, and Notion AI seemed neat. I started to plan the migration by listing my objectives. I wanted to edit in Notion, have a beautiful URL, support SSR, have a relatively good response time worldwide, and have a maximum budget of 30-50 dollars per month. The budget might seem significant for a static content website, but I could count that as an engineering experiment cost, so all is well.

I then went to the drawing board and devised several alternatives. I considered calling the Notion API directly from the client using Next.js or Remix.js, but the Notion API is limited to 1-2 API calls per second. Because the previous one was impossible, I considered using a layer between the client side and Notion. I planned to create it using Clojure as a lab. However, I stopped myself and looked back. Are there readily available solutions that give me difficulty competing in the budget department?

## Enter Super

Then I found Super ([super.so](http://super.so/)) and several options and decided to use Super instead of rolling my solution.

Why did I choose Super? It fits the bill; my data is in Notion, enabling me to use Notion AI and API. The setup is easy, and the website customization process is relatively smooth. It relieves me from creating my solution of writing platform in a hurry, gives me time to focus on writing and creating other products, and allows me to have all of the above for 16 USD per month (12 USD if you subscribe yearly).

The migration process was smooth, and I did it in under 1 hour, including setting up Google Analytics and Search Console. Even though it has limitations on its bad code block in dark mode and not supporting the mermaid code block as in the Notion, it is still acceptable. I still want to roll my solution to this problem to fully customize my website (e.g., double linking like LogSeq, e.t.c.). But so far, I am happy with the migration and recommend anyone who wants to write in Notion and create a website using it to try Super and examine whether it is an acceptable solution for you.
