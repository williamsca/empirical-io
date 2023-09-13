# Class Discussion of Mortimer (2007)

## Estimation FAQ
Will a consumer with a high marginal utility of income $\alpha_i$ be more likely to purchase or rent?
> Rent, assuming rental prices are lower than purchase prices.

Why is it useful to include so many free parameters in (2) for this industry?
> Many of a product’s important attributes, such as the quality of a star’s acting, may be unobservable.

What is the interpretation of the $\xi_{r,m,j}$ is (2)?
> The unobserved quality of a use of title $j$ in market $m$ in format $j$. For example, a rental of title $j$ may be more valuable in market $m$ because of unobservable promotions or community events.

Should we worry about endogeneity? What are plausible sources of endogeneity here? How are they addressed?
> Typical route: use IV based on variation in costs or supply-side conditions
> Variation in retailer costs depends on how many times a title is rented
> This is correlated with local demand!
> Solution: use *expected* average costs based on similar titles
> If variation driven only by cost factors (e.g., retailer impatience, managerial skill, etc.), instrument is OK
> If demand factors play a role (e.g., consumer impatience, etc.), IV estimates are biased
> Titles with unobservably high quality have higher prices, bias mean price coefficient $\gamma_0$ upward
> IV, OLS yield similar results: sticky prices $\implies$ no need to instrument, or demand-side bias in IV is large

Moment condition. $W$ includes $Z_m$ (median income) and instruments

Many parameters: 258 $\delta$s, 328 in $\beta$ vector

What is the normalization in the utility function?
> The value of the outside good for movie 1 on the VHS format is normalized so that $E[X_m\beta_1]$ is equal to a constant. This is done to preserve the geographic variation in the normalization for title 1.

What variation identifies $\gamma$ and $\rho$, which govern the distribution of the marginal utility of income $\alpha$?
1. The normalization of the quality of the first movie in the first market
2. Variation in income
3. Variation across geographies in the market shares of rentals versus sales

## Results
**Table IV**

Panel A
- Higher median incomes are associated with higher WTP for movies on tape or DVD (lower marginal utility of income $\alpha$).
- Larger stores are located in neighborhoods with greater demand for movies
- Married with kids have preference for DVDs
- Higher % DVD penetration associated with lower per-household WTP for DVDs... why? High value consumers adopted early, so later adopters drive down average demand.

Panel B
- Decay rate in quality is .89
- Renting a VHS yields ~70% of the value of purchasing, while renting a DVD yields only ~50-60%

Panel C
Calculated directly from data (not estimated)
- Rental priced VHS used most intensely, then sell-through VHS, and finally DVDs
- Rental markups highest for sell-through priced VHS

**Scatterplots**

Recall predictions: movies with (1) higher value in rental market and/or (2) slower decay in the sales market should be more likely to be rental priced

Scale each movie's quality parameter by value in the first-period sales market

Horizontal axis shows relative value of each movie in the second-period sales market (i.e., the decay rate)

Figure 1: clear support for prediction (1), more ambiguous on prediction (2)

Figure 2 limits to the 12 movies which were rental priced on VHS. Strong support: VHS titles are both relatively more valuable rentals and relatively more valuable as second period sales.


## Standard Questions

What is the research question?
   
> Are studio pricing strategies for DVDs and VHS tapes consistent with indirect price discrimination? What are the consumer welfare implications?

What are the goals of the paper? Does the paper aim to develop methods,
answer a policy question, test models, or measure an effect?

> Answer a policy question and measure an effect.

Why is the paper important according to the author? Is the author right?


If the objective is a methodological advance, what is the problem that the author addresses? Why are existing techniques inadequate? How successful is the author’s approach? Could we use the approach elsewhere?


What is the theoretical foundation for the empirical work? How appropriate is the model for the applications? How tight is the relationship between the theoretical and empirical models?

> Intertemporal price discrimiation for a durable good (Mussa and Rosen 1978). The firm destroys quality (via "rental pricing") to better sort consumers.

For papers using structural empirical models:
- Why does the author use a structural empirical model? Could any of the
questions the author asks be addressed with other approaches? What is
the source of identification? Could the model be generalized in any obvious ways?
- Does the structural model seem to capture the key features of the market? What elements are missing from the model that might be important? Is it clear what the implications of ignoring these elements are for the estimate the author obtains?
- Are there overidentifying restrictions of the model that could be tested? Can you think of alternative models of behavior for the market?

What are the data?

> DVD and VHS rental and sales transactions at video retail stores. For each title: box-office revenues, genre, and MPAA rating. Local competitive conditions from Yellow Pages. Demographic data by retailer zipcode from the 2000 Census.

What are the key variables in the empirical model?

> Product quality $\delta$, prices $p$, and the number of rentals for each tape or DVD $\tau$.

What is assumed to be exogenous and endogenous? How is the endogeneity
addressed? Do you believe the solutions?

> Quality is exogenous (and in particular, firms do not choose how long to wait before adjusting the price of a title).

What variation in the data does the author rely on for identification of each element of the empirical model? What assumptions must be true about this variation for the author’s interpretation of the results to be correct? Are you worried about any of the assumptions?

Does the author make distributional assumptions? If so, are they important for identification? Does the author evaluate the robustness of the results to the assumption?

> Consumer preferences $\alpha_i$ are assumed Weibull.

What are the conclusions of the author?

What alternative interpretations are plausible? Does the author test against any plausible alternatives, or provide any practical reason why they are less likely?

Mortimer, Julie Holland. "Price discrimination, copyright law, and technological innovation: Evidence from the introduction of DVDs." *The Quarterly Journal of Economics* 122, no. 3 (2007): 1307-1350.


