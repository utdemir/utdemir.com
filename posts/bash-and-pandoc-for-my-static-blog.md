---
title: "You don't need a static website generator"
date: 2020-02-09
draft: true
---

I like static websites. They are easy to create, deploy and maintain. Many
people agree, so there are many static website generators. For this blog,
I tried a few of them, namely [pelican](), [hakyll](), and [zola](). All
of them _worked_ after I stared at their docs long enough, but I felt
like they were a bit of an overkill for tihs simple blog.

As many others, this blog is quite simple. It starts with a bunch of
markdown files, they are passed through an HTML template, packed together
with a few stylesheets and end up linked from an index page. For bigger
blogs maybe we need pagination or tags, but usually nothing too fancy
is required.



. They are packed together
with some static css files,

As you can see, this blog (as
most others) is quite simple; there are separate .html files for each
blog post, a bunch of static .css files, and an index file linking to
each post.
