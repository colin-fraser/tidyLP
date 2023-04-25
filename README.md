
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyLP

<!-- badges: start -->

[![check-standard](https://github.com/colin-fraser/tidyLP/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/colin-fraser/tidyLP/actions/workflows/check-standard.yaml)
<!-- badges: end -->

`{tidyLP}` provides what I think is a handy and intuitive interface
between data.frames and linear programs.

## Installation

You can install the development version of tidyLP from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("colin-fraser/tidyLP")
```

## A Very Simple Example

I’ll start with a classic: tables and chairs (modified from [these
slides](http://www.cs.cmu.edu/afs/cs/academic/class/15780-s16/www/slides/linear_prog.pdf)
by Zico Kolter at CMU). A furniture company makes tables and chairs.
They are made of wood and metal. A table is made of 1 unit of metal and
3 units of wood, and when you sell a table you make a profit of \$200. A
chair is made of 2 units of metal and 1 unit of wood, and makes a profit
of \$100. We have 6,000 units of metal and 9,000 units of wood in
inventory. How many tables and chairs should we make?

### tl;dr, the solution looks like this:

``` r
library(tidyLP)
df <- tibble::tribble(
  ~ product_type,~ metal_units, ~ wood_units, ~ profit,
  'table',                   1,            3,      200,
  'chair',                   2,            1,      100
)
df |> 
  tidy_lp(
    # objective function
    profit,
    
    # constraints
    metal_units ~ leq(6000),
    wood_units ~ leq(9000)
  ) |> 
  lp_solve() |> 
  bind_solution(name = 'quantity') |> 
  select(product_type, quantity)
#> # A tibble: 2 × 2
#>   product_type quantity
#>   <chr>           <dbl>
#> 1 table            2400
#> 2 chair            1800
```

### Details

To start solving the problem with `{tidyLP}`, we need a data frame that
expresses the information we need for the objective function and the
constraints. This data frame should have the same number of rows as
there are variables in the objective function. In this case, we are
choosing a number of tables and a number of chairs, so the data frame
should have two rows.

``` r
df <- tibble::tribble(
  ~ product_type, ~ metal_units, ~ wood_units, ~ profit,
  'table', 1, 3, 200,
  'chair', 2, 1, 100
)
df
#> # A tibble: 2 × 4
#>   product_type metal_units wood_units profit
#>   <chr>              <dbl>      <dbl>  <dbl>
#> 1 table                  1          3    200
#> 2 chair                  2          1    100
```

To build the linear programming problem, we use the `tidy_lp` function,
whose signature looks like this:

    tidy_lp(
      .data,
      .objective,
      ...,
      .direction = "max",
      .all_int = FALSE,
      .all_bin = FALSE
    )

In its first argument, it takes the data frame. The second argument is
reserved for the objective function, which can be specified just by
naming a column from the data frame—in this case, we want to maximize
profit, so that’s what it will be. Then we pass constraints as unnamed
arguments to `...` in a special format that uses some formula trickery.
With the data frame formatted correctly, I find that it’s quite natural
to formulate the problem with this interface. We want to do the
following

- Max profit
- Use less than or equal to 6,000 metal units
- Use less than or equal to 9,000 wood units

Using `tidy_lp`:

``` r
library(tidyLP)
lp_problem <- tidy_lp(df,
                      profit,
                      metal_units ~ leq(6000),
                      wood_units ~ leq(9000))
```

The constraints are specified using a combination of formulas and some
special helper functions—primarily, `leq`, `geq`, and `eq`, which are
used on the right hand side of the formula to specify the direction of
the constraint.

This builds the linear program, but does not solve it. To solve it, you
pass `lp_problem` to `lp_solve`.

``` r
solution <- lp_solve(lp_problem)
```

It’s convenient to bind the solution to the original data frame, which
puts the solution in a column called `.solution`.

``` r
library(dplyr)
bind_solution(solution) |> 
  select(product_type, .solution)
#> # A tibble: 2 × 2
#>   product_type .solution
#>   <chr>            <dbl>
#> 1 table             2400
#> 2 chair             1800
```

Just to be sure, let’s have a closer look at the solution.

``` r
bind_solution(solution) |> 
  summarise(across(c(metal_units, wood_units, profit), ~sum(.x * .solution)))
#> # A tibble: 1 × 3
#>   metal_units wood_units profit
#>         <dbl>      <dbl>  <dbl>
#> 1        6000       9000 660000
```

### More complexity in the left hand side of the constraints

In addition to the constraints laid out above, there are a few more
details.

1.  We only have enough warehouse space to store 1,000 tables.
2.  Chairs can be bought individually, but each table is sold with two
    chairs. We need to make sure to produce enough chairs that they can
    be bundled with the tables.

To specify these constraints in `tidyLP`, we can include column
transformations on the left hand side of the constraint formula. Under
the hood, whatever is on the left hand side of the constraint formula is
passed through the data using `dplyr::transmute`, so anything that would
work there works here. Thus, the first constraint can be specified as
follows.

``` r
lp_problem <- lp_problem |> 
  add_constraints( 
    (product_type == 'table') ~ leq(1000)
  )
```

The second constraint can be built similarly. We need
`chairs >= 2 * tables`. In `tidyLP`, constraints must be specified with
a numeric right hand side, so rearrange this to give
`chairs - 2 * tables >= 0`.

``` r
lp_problem <- lp_problem |> 
  add_constraints(
    (product_type == 'chair') - 2 * (product_type == 'table') ~ geq(0)
  )

new_solution <- lp_problem |> 
  lp_solve() |> 
  bind_solution()
new_solution
#> # A tibble: 2 × 5
#>   product_type metal_units wood_units profit .solution
#>   <chr>              <dbl>      <dbl>  <dbl>     <dbl>
#> 1 table                  1          3    200      1000
#> 2 chair                  2          1    100      2500
```

These constraints really eat into our profits!

``` r
new_solution |> 
  summarise(across(c(metal_units, wood_units, profit), ~sum(.x * .solution)))
#> # A tibble: 1 × 3
#>   metal_units wood_units profit
#>         <dbl>      <dbl>  <dbl>
#> 1        6000       5500 450000
```

## A more complex example: fantasy sports

The package includes a (modified, fake, DO NOT USE FOR REAL FANTASY TEAM
BUILDING) fantasy basketball dataset.

``` r
head(fantasy_points)
#> # A tibble: 6 × 6
#>   Id          Position Name                  Projected_FP Salary Team 
#>   <chr>       <chr>    <chr>                        <dbl>  <dbl> <chr>
#> 1 82696-84669 PG       Luka Doncic                   64.6  11800 DAL  
#> 2 82696-40199 PF       Giannis Antetokounmpo         60.1  11600 MIL  
#> 3 82696-15755 PF       Anthony Davis                 51.1  10500 LAL  
#> 4 82696-80808 PF       Jayson Tatum                  47.9  10300 BOS  
#> 5 82696-9488  SF       LeBron James                  49.8  10200 LAL  
#> 6 82696-84671 PG       Trae Young                    45.8  10000 ATL
```

In fantasy basketball (Fanduel rules), the problem is to assemble a team
of 9 players that scores the most fantasy points, staying under the
salary cap of 60,000. This can be expressed as a binary linear program.
A naive first attempt with `tidyLP` looks like this.

``` r
team <- fantasy_points |> 
  tidy_lp(
    Projected_FP,  # objective function
    Salary ~ leq(60000),
    all_variables() ~ eq(9),
    .all_bin = TRUE  # indicates this is a binary LP
  ) |> 
  lp_solve() |> 
  bind_solution(filter_nonzero = TRUE) # only show rows where the solution is non-zero

team |> 
  select(Name, Position, Projected_FP, Salary)
#> # A tibble: 9 × 4
#>   Name                  Position Projected_FP Salary
#>   <chr>                 <chr>           <dbl>  <dbl>
#> 1 Luka Doncic           PG               64.6  11800
#> 2 Giannis Antetokounmpo PF               60.1  11600
#> 3 Terry Rozier          SG               47.7   7800
#> 4 Fred VanVleet         PG               49     7500
#> 5 Scottie Barnes        PF               49     6700
#> 6 Ricky Rubio           PG               30.3   3500
#> 7 Jaren Jackson         PF               32.9   3500
#> 8 Robert Williams       PF               31.1   3500
#> 9 Lonzo Ball            PG               32.9   3500
```

Just to double check, let’s make sure this team satisfies the salary cap
constraint.

``` r
team |> 
  summarise(sum(Salary))
#> # A tibble: 1 × 1
#>   `sum(Salary)`
#>           <dbl>
#> 1         59400
```

✅

However, this is not actually a legal solution, because I’ve left out
some important constraints. There are constraints around the
distribution of positions: a legal team has two each of point guards,
shooting guards, small forwards, and power forwards; and one center. We
can use the same methods from the tables and chairs example to specify
these constraints.

``` r
team <- fantasy_points |> 
  tidy_lp(
    # objective function
    Projected_FP,
    
    # constraints
    Salary ~ leq(60000),
    (Position == 'PG') ~ eq(2),
    (Position == 'SG') ~ eq(2),
    (Position == 'SF') ~ eq(2),
    (Position == 'PF') ~ eq(2),
    (Position == 'C') ~ eq(1),
    
    # additional arguments
    .all_bin = TRUE
  ) |> 
  lp_solve() |> 
  bind_solution(filter_nonzero = TRUE)

team |> 
  select(Name, Position, Projected_FP, Salary)
#> # A tibble: 9 × 4
#>   Name              Position Projected_FP Salary
#>   <chr>             <chr>           <dbl>  <dbl>
#> 1 Donovan Mitchell  SG               50.9   9500
#> 2 Pascal Siakam     C                50.8   9500
#> 3 Terry Rozier      SG               47.7   7800
#> 4 Brandon Ingram    SF               36.5   7600
#> 5 Fred VanVleet     PG               49     7500
#> 6 Scottie Barnes    PF               49     6700
#> 7 Bogdan Bogdanovic SF               27.2   4000
#> 8 Jaren Jackson     PF               32.9   3500
#> 9 Lonzo Ball        PG               32.9   3500
```

Just to make sure, let’s check that the constraints are satisfied.

``` r
team |> 
  count(Position)
#> # A tibble: 5 × 2
#>   Position     n
#>   <chr>    <int>
#> 1 C            1
#> 2 PF           2
#> 3 PG           2
#> 4 SF           2
#> 5 SG           2
```

``` r
team |> 
  summarise(sum(Salary))
#> # A tibble: 1 × 1
#>   `sum(Salary)`
#>           <dbl>
#> 1         59600
```

### Categorical Constraints

However, there’s yet another set of constraints: we can only have at
most 2 players from any single NBA team.

``` r
team |> 
  count(Team)
#> # A tibble: 7 × 2
#>   Team      n
#>   <chr> <int>
#> 1 ATL       1
#> 2 CHA       1
#> 3 CHI       1
#> 4 CLE       1
#> 5 MEM       1
#> 6 NO        1
#> 7 TOR       3
```

This solution has too many Raptors. One way to specify these constraints
is to continue the same way that we specified the position constriants.

``` r
fantasy_points |> 
  tidy_lp(
    # objective function
    Projected_FP,
    
    # constraints
    Salary ~ leq(60000),
    (Position == 'PG') ~ eq(2),
    (Position == 'SG') ~ eq(2),
    (Position == 'SF') ~ eq(2),
    (Position == 'PF') ~ eq(2),
    (Position == 'C') ~ eq(1),
    
    (Team == 'ATL') ~ leq(2),
    (Team == 'BOS') ~ leq(2),
    ...
    
```

But this would be very tedious. Instead, we can use another trick, which
is to specify a `categorical_constraint`. A `categorical_constraint` is
actually a set of constraints, one for each unique value in the
specified column.

``` r
team <- fantasy_points |> 
  tidy_lp(
    # objective function
    Projected_FP,
    
    # constraints
    Salary ~ leq(60000),
    (Position == 'PG') ~ eq(2),
    (Position == 'SG') ~ eq(2),
    (Position == 'SF') ~ eq(2),
    (Position == 'PF') ~ eq(2),
    (Position == 'C') ~ eq(1),
    categorical_constraint(Team) ~ leq(2),
    
    # additional arguments
    .all_bin = TRUE
  ) |> 
  lp_solve() |> 
  bind_solution(filter_nonzero = TRUE)

team |> 
  select(Name, Position, Projected_FP, Salary, Team)
#> # A tibble: 9 × 5
#>   Name              Position Projected_FP Salary Team 
#>   <chr>             <chr>           <dbl>  <dbl> <chr>
#> 1 Luka Doncic       PG               64.6  11800 DAL  
#> 2 Donovan Mitchell  SG               50.9   9500 CLE  
#> 3 Pascal Siakam     C                50.8   9500 TOR  
#> 4 Terry Rozier      SG               47.7   7800 CHA  
#> 5 Scottie Barnes    PF               49     6700 TOR  
#> 6 Bogdan Bogdanovic SF               27.2   4000 ATL  
#> 7 Jaren Jackson     PF               32.9   3500 MEM  
#> 8 Danilo Gallinari  SF               20.5   3500 BOS  
#> 9 Lonzo Ball        PG               32.9   3500 CHI
```
