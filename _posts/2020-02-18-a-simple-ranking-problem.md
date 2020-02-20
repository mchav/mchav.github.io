---
layout: post
title: A simple ranking problem
---

Suppose you ran a Yelp-like website where users entered ratings for places
they've been to. The ratings are on a 5 point scale. Suppose further that our
dataset contains only three columns: the place being rated, the average 
rating of the place, and the number of ratings that place has received.

| Place         | Rating  | Number of ratings  |
|---------------|---------|--------------------|
| Restaurant A  |   5.0   |       400          |
| Restaurant B  |   4.5   |       130          |
| Restaurant C  |   4.7   |         2          |
| Restaurant D  |   3.3   |       500          |
| Restaurant E  |   2.8   |       160          |
| Restaurant F  |   3.8   |        10          |
| Restaurant G  |   1.3   |      1000          |
| Restaurant H  |   1.8   |       190          |
| Restaurant I  |   2.0   |        30          |

How do we sort this list of places from best to worst?


One idea would be to sort only by the rating. This idea quickly proves to be
unhelpful. Not all 5* ratings are equal. A 5* rating from 1000 ratings is more
likely to be better quality than a 5* place with 1 rating. The number of ratings
acts as a confidence score for each rating. Hence, our ranking function should
somehow incorporate this information.


As a second attempt, let's modify the ranking function to be the product of the
rating and the number of ratings. That way a 5* rating from 1 user ranks lower
than a 4* rating from 100 ratings. This approach is still unsatisfactory. A 1*
rating from 10'000 users would rank over a 5* rating from 1'000 users. A
reasonable ranking function wouldn't permit that.


The product of the average rating and the number of ratings captures a good
intuition however. The number of ratings acts as confidence score of the rating.
It shouldn't, however, inflate low ratings. We need to limit the influence of
number of ratings in our function. Since the number of ratings acts primarily as
a confidence score we can normalise it to lie in a fixed range. In this example
we'll normalise the number of ratings to some value between 1 and 5. We map
number of ratings to our new normalised score using quantiles. The 0th to 20th
percentlie number of ratings will map to the first bucket. The 21st to 40th
percentile will map to the second bucket and so on.


Our final score will be the product of the two values. Going back to our
previous problem where the 1* rated place ranked first from sheer volume - . In the
case where the number of ratings is in the 80th to 100th percentile, the
total score for the place is 5 - which is pretty low on a 25 point ranking scale.
