# ernie

[PERT](https://en.wikipedia.org/wiki/Program_evaluation_and_review_technique) Project planning in Haskell. Generates a PERT chart and computes the critical path based on simulations.

![docs/example.svg](docs/example.svg)

## Usage

* There is a CLI that takes task lists (in JSON) and produces PERT charts (in DOT format). The simplest way to run it is `ernie -f project.json`. See [example](https://github.com/j-mueller/ernie/tree/main/example) folder for details.
* You can also use it as a Haskell library, see [`Ernie.Example`](https://github.com/j-mueller/ernie/blob/main/src/ernie/lib/Ernie/Example.hs).

## Design Choices

Task durations are entered as PERT estimates, but internally we treat them as probability distributions. As a result we get a distribution of possible realisations of the entire project (this distribution can be sampled from using [`Ernie.Sample.sample`](https://github.com/j-mueller/ernie/blob/main/src/ernie/lib/Ernie/Sample.hs)). The probabilistic approach has some consequences for how we calculate durations.

1. The *critical path* in a PERT chart is usually defined as the longest path from start to finish. The name comes from the fact that a delay in a task on this critical path invariably results in a delay of the entire project. However, if you think about a diamond-shaped dependency (like the example above), it is clear that either of the two tasks in the middle can be on the critical paths, depending on which of them takes longer. So in the probabilistic setting the critical path is a distribution, not a single path. We account for this by computing a "probability of being on the critical path" for each task. This is the _CP_ value in the charts.
2. PERT assumes that *minimum and maximum values* for task durations can be determined. From a risk management perspective it doesn't make sense to specify absolute min/max values for task durations - anyone who has ever done any programming knows that the true upper bound of every task is infinity (and the lower bound is 0). Some people use estimates of the 5th and 95th quantile. This is generally preferrable, but in the context of PERT, those estimates are then transformed into a regular PERT estimate with upper and lower bounds (see for example [this blog](https://towardsdatascience.com/python-scenario-analysis-modeling-expert-estimates-with-the-beta-pert-distribution-22a5e90cfa79)). The [transformation that's currently used](https://github.com/j-mueller/ernie/blob/main/src/ernie/lib/Ernie/PERT.hs#L57-L69) in `ernie` still has the finite boundaries problem but I'm hoping to tackle it soon, together with the addition of some actual risk measures for project duration.

## Features (planned)

* Critical path: milestones
* Project start / end / duration
* Risk measures
* Export samples to CSV

Maybe features

* Resource planning (maybe?)
* Combine plan with progress data

## Packages

* `ernie` Implements PERT charts
* `floyd-warshall` Implements the [Floyd-Warshall algorithm](https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm). Based on a [blog post by Russell O'Connor](r6.ca/blog/20110808T035622Z.html)

## Contributing

Contributions are welcome!
