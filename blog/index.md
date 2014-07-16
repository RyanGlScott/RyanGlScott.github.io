---
layout: default
title: Blog
---

<h1 style="text-align:left;float:left;">Blog</h1> 
<h1 style="text-align:right;float:right;"><a href="{{ site.baseurl }}/feed.xml">RSS feed</a></h1>
<div style="clear:both;"></div>

> Disclaimer: All opinions expressed here represent my own and not those of my employer(s).

<section class="archive">
{% for post in site.posts %}
{% unless post.next %}

{% unless forloop.first %}</div></div>{% endunless %}

  <div class="bundle row gutters fadeInDown animated">
    <h2 class="post-year col span_2">{{ post.date | date: '%Y' }}</h2>
    <div class="posts-by-year col span_10">

{% else %}
{% capture year %}{{ post.date | date: '%Y' }}{% endcapture %}
{% capture nyear %}{{ post.next.date | date: '%Y' }}{% endcapture %}
{% if year != nyear %}

{% unless forloop.first %}</div></div>{% endunless %}

  <div class="bundle row gutters fadeInDown animated">
    <h2 class="post-year col span_2">{{ post.date | date: '%Y' }}</h2>
    <div class="posts-by-year col span_10">
{% endif %}
{% endunless %}

  <article class="row gutters">
    <a href="{{ site.baseurl }}{{ post.url }}" title="{{ post.title }}" class="col span_8">{{ post.title }}</a>
    <div class="post-date col span_4">
      <time datetime="{{ post.date | date: '%Y-%m-%d' }}">{{ post.date | date: "%B %-d" }}</time>
    </div>
  </article>

{% if forloop.last %}</div></div>{% endif %}

{% endfor %}
</section>
