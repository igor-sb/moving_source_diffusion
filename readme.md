# Diffusion from a moving source

_(this was a side project I did during my PhD)_

![Neutrophil chasing bacteria](https://github.com/igor25/moving_source_diffusion/blob/master/videos/neutrophil_chasing_bacteria.gif)

Many living cells eat and digest other living cells. One example is shown in the
video below taken from a 16 mm movie made in 1950s by the late prof. David Rogers
at Vanderbilt University. Here a white blood cell (largest thing moving around) is crawling among red blood cells (circular objects) and chasing a _Staphylococcus aureus_ ("Staph") bacteria. Bacteria secretes chemicals that white blood cells can sense and use a
"trace" to find and eat bacteria.

I was curious what is the best possible accuracy with cells (such as white blood cells) can track chemical gradients generated by other cells. This seems difficult given that the
detection of chemical signals is very noisy and that chemicals also diffuse and
spread in the environment, making the problem very challenging on a microscope scale. To put it into perspective, if you manually introduce a small patch of chemical the size of bacteria
in the video, in one second it would spread over the area of a white blood cell (~ 20 &mu;m by 10 &mu;m)
