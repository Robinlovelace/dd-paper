Distance decay functions for modelling cycling uptake
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Abstract

# Introduction

Trips of short distances tend to be more frequent than trips of long
distances. This is a fundamental feature of transport behaviour,
observed in the majority of transport systems worldwide. The concept is
neatly captured by the term *distance decay* (*dd*), meaning simply that
the proportion of trips of any given type tends to zero as distance
tends to infinity.

*Distance decay* — along with the synonymous *deterrence function*
(Simini et al. 2012) — builds on older terms expressing the same
fundamental truth, such as the ‘friction of distance’ and the ‘gravity
model’ (described in the subsequent section). Work focussed explicitly
on *d**d* explores *how* the proportion of trips (*p*) declines with
distance (*d*) (Iacono, Krizek, and El-Geneidy 2008), beyond the simple
observation that the slope is negative beyond a certain threshold
distance. A resergence of interest in the distance-frequency
relationship has refined and, in some cases, sought to overturn
pre-existing (and often context-specific) formulations, such as
$p = \\\\alpha e^{\\\\beta d}$ (A. Wilson 1971) and
$p = \\\\alpha d^{\\\\beta}$ (Fotheringham 1981). Much *dd* research
asks: what is the best function for linking the proportion (different
types of) trip to distance?

Mathematically, what is *f* of *x*

*p* = *f*(*x*)

where *x* is distance and *p* is the proportion of trips made in any
distance band? Distance decay models can be empirically tested, and
therefore ranked, by comparing observed travel behaviour with the levels
expected based on different distance decay functions.

Equation 1.1 can clearly be refined in many directions. Subscripts can
be added to the terms and new variables can be added to illustrate how
distance decay changes with a range of additional factors such as type
of bicycle and hillness of the route. how distance decay changes in
response to a range of variables. The type of trip (e.g. time of day,
purpose), characteristics of the people making the trip (e.g. age and
sex) and the location and physical surroundings of the trip (e.g.
hilliness, transport infrastructure) have all been found to affect the
shape of distance decay curves \[@[Fotheringham1981;Fingleton
(2005)](mailto:Fotheringham1981;@Fingleton2005)\]. Critically for
understanding transport systems is the *mode*, or ‘method’ of transport.
Mode refers to the choice of ‘vehicle’ (e.g. walking, cycling, car
driving) and has been found, unsurprisingly, to dramatically affect the
speed travelled during and amount of effort required for trips of
different distances. The *dd* concept is especially applicable to active
travel modes due to physiological limits on human mechanical power
output and the slow speed of these modes. The next section shows that
the concept has a long history in the academic literature but that
interest in the term from the perspective of walking and cycling is
relatively recent.

# The distance decay literature

A variety of terms have been used to express the idea underlying the
*d**d* concept outlined in this paper. These including the ‘first law of
geography,’ the ‘friction of distance’ and ‘the gravity model.’ It is
worth reviewing these terms before describing work refering to *d**d*
directly, to put the term in its wider historical context. The final
part of this section reviews recent literature explicitly using the term
‘distance decay’ to explore the concept

## The gravity model

The ‘gravity model’ of movement patterns helped to quantify and
generalise early incarnations if *dd* (Zipf et al. 1946). This rule (it
is sometimes referred to as the ‘gravity law’) suggests that travel
incentives are roughly analogous to Newtonian gravitation (Ravenstein
1885). The resulting formulae imply that the total movement between two
places (*T*\_*i**j* between origin *i* and destination *j*) is
proportional to the product of their populations (*m*<sub>*i*</sub> and
*n*<sub>*j*</sub>), divided by a power of the distance between them:

$$
T\_{ij} = \\frac{m\_i n\_j}{d^\\beta}
$$

as it has sometimes been called has been a rich source of theoretical
and methodological advance in many fields, primarily urban modelling but
also in fields as diverse as highway planning (Jung, Wang, and Stanley
2008), national transport of minerals (Zuo et al. 2013) and spatial
epidemiology (Balcan et al. 2009).

## The radiation model

Despite dissenting voices — including the statement that “a strict
gravity model simply did not work” for modelling urban systems and that
some subsequent refinements to the gravity model were “fudge factors”
(A. A. A. G. Wilson 1998) — the gravity model has been one of the
dominant tools for understanding urban mobility over the past 100 years
(Masucci et al. 2013). A recent development in this field has been the
‘radiation model,’ which has been found to fit travel to work and other
flow data well (Simini et al. 2012). This new formula for estimating
flow rates between geographic zones is interesting in its ommission of
distance as an explicit explanatory variable. Instead, the radiation
model uses the number ‘intervening opportunities’ (*I**O*) as a proxy
for *d**d* the denominator to estimate flow:

$$ dd \\\\approx (m\_i+s\\\_{ij})(m\_i+n\_j+s\\\_{ij}) $$

A recent study compared the parameter-free radiation model against the
gravity model on a large intra-city (London) dataset on commuting. It
was found that neither model produced a satisfactory fit with the data,
leading to the conclusion that “commuting at the city scale still lacks
a valid model and that further research is required to understand the
mechanism behind urban mobility” (Masucci et al. 2013). The ‘first law
of geography’ and the related concept of the ‘friction of distance’ are
alternative yet closely related (i.e. not mutually exclusive) terms for
exploring *d**d* th

## The first law of geography

Tobler’s famous **first law of geography** states that “everything is
related to everything else, but near things are more related than
distant things” (Tobler 1970). The phrase implies that interaction
between things declines with distance without specifying how or why.
Clearly, the increased frequency of communications and transport between
places that are close to each other can help explain spatial
autocorrelation (Miller et al. 2004). However, unlike *d**d*, the ‘first
law’ says little about the way in which relatedness between entities
declines with distace. Moreover, in a world of accelerating
globalisation under the auspices of the ongoing ‘digital revolution,’
the relevance of the ‘first law’ to the system-level processes it was
proposed to explain has come under scrutiny (Westlund 2013).

To overcome this issue, the scope of this paper is limited to
transportation of people and goods, opposed to immaterial communication.
This simplifies the issue and makes it more tangible. Due to fundament
physical limits on the efficiency with which matter can be moved (MacKay
2013), and a limited supply of energy (especially pertinent in an era of
resource over-extraction and climate change), such physical transport
will always be limited to some degree by distance. This notion of travel
frequency tending to zero as distance tends to infinity, present in the
‘friction of distance’ terminology, is also encapsulated by the more
recent phrase ‘distance decay’ used in this paper.

## The friction of distance

The concept of the ‘friction of distance’ has been in use for over 100
enjoying steady (albeit slowing) growth in use until the early 2000s
(Fig. 1). The term helps to explain why Tobler’s Law is true, implying
that it is more *difficult* (e.g. in terms of energy use) for distant
things to interact. ‘Friction’ is thus defined as “the difficulty of
moving a volume (passengers or goods)” (Muvingi 2012). The friction of
distance is predominantly used as a synonym for *d**d*, and to some
extent the slowing use of the term illustrated in Fig. 1 can be seen as
a consequence of the latter simply replacing the former. One study, for
example, refers to “the friction of distance parameter in gravity
models” (Cliff, Martin, and Ord, n.d.) whereas others refer to ‘distance
decay’ in spatial interaction and gravity models respectively: the terms
are largely interchangeable \[Griffith and Jones (1980);McCall2006\].
However, the question of precisely *how* the metaphorical friction
increases with distance has been tackled to a lesser extent in the
(generally older) literature using the term. The recent *d**d*
literature, by contrast, is largely focussed this question, as described
in the subsequent section.

## Recent distance decay literature

As illustrated in Figure \\ref{fcitations}, *d**d* has grown rapidly as
a term in the academic literature over the past 50 years, compared with
the well-established “gravity model” terminology. By the 1970s, it seems
that *d**d* overtook the “friction of distance” amongst transport
researchers. Although Tobler’s ‘First Law’ of geography has gained rapid
acceptance since its inception in the 1970s, it has a far lower (by a
factor of 5) rate of use than *d**d* in the transport literature. In
other words, a quantitative review of the literature demonstrates that
*d**d* is an up-and-coming term in transport studies. This section does
not attempt a full overview of the 10,000+ articles using the *d**d*
terminology in the academic literature, instead focussing on a few key
and highly cited works that helped set the agenda for *d**d* research.

“friction of distance” terms. (which tends to assign importance to
interaction between places rather than the impact of distance)

![Number of citations per decade for different terms for ‘distance
decay’ from Google
Scholar.](README_files/figure-gfm/unnamed-chunk-7-1.png)

Within the general understanding that trips become less frequent with
distance (beyond some threshold limit) there are various ways of
describing the dependent variable that *dd* functions seek to explain.
*dd* can be understood as:

-   the absolute number of trips expected for any given distance band
    (*d**b*): *f*(*d*) = *T*\_*d**b*
-   the proportion of *all* trips that are made for a given distance
    band: *f*(*d*) = *T*\_*d**b*/*T*
-   the proportion of trips *within a given distance band* made by a
    particular trip type (e.g. walking): \\newline
    *f*(*d*) = *T**w**a**l**k*\_*d**b*/*T*\_*d**b*

Of these definitions the second is the most generalisable, being a
proportion ($0 \\\\geq p \\\\leq 1$) that tends to zero with increasing
distance for any mode of transport. The third definition of *dd* also
estimates a proportion with the same constraints, but is less
generalisable: the result will not always tend to zero (the proportion
of trips by air travel, for example, will tend to one for large trip
distances). The first and simplest definition is the least generalisable
as it is highly dependent on the total amount of travel.

# Functional forms of distance decay

Four functional forms commonly used in the literature to characterise
distance decay curves were described in a recent paper (Mart’ınez and
Viegas 2013) <!-- [@Martinez2013]  --> as:

-   Exponential functions, $e^{\\\\beta x}$ (A. Wilson 1971).
-   Power functions, $x^{\\\\beta}$ (Fotheringham 1981).
-   Tanner functions, $x^{\\\\beta\_1} e^{\\\\beta\_2 x}$
    (Openshaw 1977).
-   Box-Cox functions,
    $exp(\\\\beta \\\\frac{x^{\\\\gamma} - 1}{\\\\gamma})$ when the
    parameter $\\\\gamma \\\\neq 0$ and $x^{\\\\beta}$ when
    $\\\\gamma = 0$

In addition to these, we have added the additional functions:

-   Modified beta distributions, which have been used to model distance
    decay in ecological research (**nekola\_scale\_2014?**)
-   A modified version of the generalised logistic or ‘Richards’
    function

$$ f(x) = \\frac{1}{(1 + Qe^{-B(x - M)})^{1/v}} $$

Each of the 4 parameters in this model can be altered to ensure optimal
fit and each has a separate meaning:

-   *B* is analogous to *β* in the other functions
-   *Q* represents the intercept
-   *v* affects the skewness of the curve
-   *M* is the *x* value of maximum growth

## A linear model

The simplest model to fit to the data is a linear model. For
illustrative purposes we will fit a linear model to the data presented
in Figure 1, with different intercepts and gradients for each of the
groups (Figure 2):

The intercepts and gradients for each group are presented in table !!!

The above numbers are equations that describe the relationship between
distance and *clc* for each group. In the `Mal_Young_NC` group, for
example,

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Balcan2009" class="csl-entry">

Balcan, Duygu, Vittoria Colizza, Bruno Gonçalves, Hao Hu, José J
Ramasco, and Alessandro Vespignani. 2009. “<span
class="nocase">Multiscale mobility networks and the spatial spreading of
infectious diseases.</span>” *Proceedings of the National Academy of
Sciences of the United States of America* 106 (51): 21484–89.
<https://doi.org/10.1073/pnas.0906910106>.

</div>

<div id="ref-cliff1974evaluating" class="csl-entry">

Cliff, A D, R L Martin, and J K Ord. n.d. “<span
class="nocase">Evaluating the friction of distance parameter in gravity
models</span>.” *Regional Studies: The Journal of the Regional Studies
Association* 8 (3-1): 281–86.
<https://doi.org/doi:10.1080/09595237400185281>.

</div>

<div id="ref-Fingleton2005" class="csl-entry">

Fingleton, Bernard. 2005. “<span class="nocase">Beyond neoclassical
orthodoxy: a view based on the new economic geography and UK regional
wage data</span>.” *Papers in Regional Science* 84.3: 351–75.
<http://onlinelibrary.wiley.com/doi/10.1111/j.1435-5957.2005.00039.x/full>.

</div>

<div id="ref-Fotheringham1981" class="csl-entry">

Fotheringham, AS. 1981. “<span class="nocase">Spatial structure and
distance-decay parameters</span>.” *Annals of the Association of
American Geographers* 71 (3): 425–36.
<http://www.tandfonline.com/doi/abs/10.1111/j.1467-8306.1981.tb01367.x>.

</div>

<div id="ref-Griffith1980" class="csl-entry">

Griffith, Daniel A, and Kelvyn G Jones. 1980. “<span
class="nocase">Explorations into the relationship between spatial
structure and spatial interaction</span>.” *Environment and Planning A*
12 (2): 187–201.

</div>

<div id="ref-Iacono2008" class="csl-entry">

Iacono, Michael, Kevin Krizek, and Ahmed El-Geneidy. 2008. “<span
class="nocase">Access to Destinations: How Close is Close Enough?
Estimating Accurate Distance Decay Functions for Multiple Modes and
Different Purposes</span>,” 76.
[http://www.lrrb.org/PDF/200811.pdf\\backslashnhttp://www.cts.umn.edu/access-study/research/6/index.html](http://www.lrrb.org/PDF/200811.pdf\backslashnhttp://www.cts.umn.edu/access-study/research/6/index.html).

</div>

<div id="ref-Jung2008" class="csl-entry">

Jung, Woo-Sung, Fengzhong Wang, and H Eugene Stanley. 2008. “<span
class="nocase">Gravity model in the Korean highway</span>.” *EPL
(Europhysics Letters)* 81 (4): 48005.

</div>

<div id="ref-MacKay13082013" class="csl-entry">

MacKay, David J C. 2013. “<span class="nocase">Solar energy in the
context of energy use, energy transportation and energy storage</span>.”
*Philosophical Transactions of the Royal Society* 371 (1996).
<https://doi.org/10.1098/rsta.2011.0431>.

</div>

<div id="ref-martinez_new_2013" class="csl-entry">

Mart’ınez, L. Miguel, and Jos’e Manuel Viegas. 2013. “A New Approach to
Modelling Distance-Decay Functions for Accessibility Assessment in
Transport Studies.” *Journal of Transport Geography* 26: 87–96.
<https://doi.org/10.1016/j.jtrangeo.2012.08.018>.

</div>

<div id="ref-masucci2013gravity" class="csl-entry">

Masucci, a. Paolo, Joan Serras, Anders Johansson, and Michael Batty.
2013. “<span class="nocase">Gravity versus radiation models: On the
importance of scale and heterogeneity in commuting flows</span>.”
*Physical Review E* 88 (2): 22812.
<https://doi.org/10.1103/PhysRevE.88.022812>.

</div>

<div id="ref-Miller2004" class="csl-entry">

Miller, Eric J, John Douglas Hunt, John E Abraham, and Paul a Salvini.
2004. “<span class="nocase">Microsimulating urban systems</span>.”
*Computers, Environment and Urban Systems* 28 (1-2): 9–44.
<https://doi.org/10.1016/S0198-9715(02)00044-3>.

</div>

<div id="ref-Muvingi2012" class="csl-entry">

Muvingi, Onai. 2012. “<span class="nocase">Restructuring air transport
to meet the needs of the Southern African development community</span>.”

</div>

<div id="ref-Openshaw1977" class="csl-entry">

Openshaw, S. 1977. “<span class="nocase">Optimal zoning systems for
spatial interaction models</span>.” *Environment and Planning A* 9 (2):
169–84. <https://doi.org/10.1068/a090169>.

</div>

<div id="ref-Ravenstein1885" class="csl-entry">

Ravenstein, E G. 1885. “<span class="nocase">The law of retail
gravity</span>.” New York: WJ Reily.

</div>

<div id="ref-Simini2012" class="csl-entry">

Simini, Filippo, Marta C González, Amos Maritan, and Albert-László
Barabási. 2012. “<span class="nocase">A universal model for mobility and
migration patterns.</span>” *Nature*, February, 8–12.
<https://doi.org/10.1038/nature10856>.

</div>

<div id="ref-Tobler1970" class="csl-entry">

Tobler, Waldo R. 1970. “<span class="nocase">A computer movie simulating
urban growth in the Detroit region</span>.” *Economic Geography*,
234–40.

</div>

<div id="ref-Westlund2013" class="csl-entry">

Westlund, Hans. 2013. “<span class="nocase">A brief history of time,
space, and growth: Waldo Tobler’s first law of geography
revisited</span>.” *The Annals of Regional Science* 51 (3): 917–24.
<https://doi.org/10.1007/s00168-013-0571-3>.

</div>

<div id="ref-Wilson1971" class="csl-entry">

Wilson, AG. 1971. “<span class="nocase">A family of spatial interaction
models, and associated developments</span>.” *Environment and Planning*
3 (January): 1–32.
<http://www.environment-and-planning.com/epa/fulltext/a03/a030001.pdf>.

</div>

<div id="ref-Wilson1998-past" class="csl-entry">

Wilson, AG Author A G. 1998. “<span class="nocase">Land-use/transport
interaction models: Past and future</span>.” *Journal of Transport
Economics and Policy* 32 (1): 3–26.
<http://www.jstor.org/stable/10.2307/20053753>.

</div>

<div id="ref-zipf1946p1" class="csl-entry">

Zipf, George Kingsley, Intercity Movement, Persons Author, George
Kingsley, and Zipf Source. 1946. “<span class="nocase">The P1 P2/D
hypothesis: On the intercity movement of persons</span>.” *American
Sociological Review* 11 (6): 677–86.

</div>

<div id="ref-Zuo2013" class="csl-entry">

Zuo, Chengchao, Mark Birkin, Graham Clarke, Fiona McEvoy, and Andrew
Bloodworth. 2013. “<span class="nocase">Modelling the transportation of
primary aggregates in England and Wales: exploring initiatives to reduce
CO2 emissions</span>.” In *IGU Conference 2013: Applied GIS and Spatial
Modelling, Hosted by the Centre for Spatial Analysis and Policy, School
of Geography, University of Leeds*, 34:112–24. Leeds: Elsevier Ltd.
<https://doi.org/10.1016/j.landusepol.2013.02.010>.

</div>

</div>
