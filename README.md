![](www/ka_logo_long.jpg)

***

This repository contains the materials used to create our submission to the [2021 RStudio Tables Contest](https://blog.rstudio.com/2021/09/30/rstudio-table-contest-2021/).

For this year's contest, we decided to make a table submission that aligned with the recent v0.4.0 release of the **{migrate}** R package for building *state transition matrices*. If you are not familiar with state transition matrices, they are a common type of data visualization used in risk management (we recommend checking out [**{migrate}**'s GitHub repository](https://github.com/mthomas-ketchbrook/migrate) to learn more).

You can view our blog post about how we built beautiful state transition matrices (and applied some tricky conditional formatting) using **{migrate}** + [**{gt}**](https://gt.rstudio.com/):

[https://mthomas-ketchbrook.github.io/migrate_table/](https://mthomas-ketchbrook.github.io/migrate_table/)

### Custom Conditional Formatting

A somewhat complicated aspect to conditionally formatting these transition matrices is that the formatting needs to be applied to a varying number of cells across all columns. We developed a [custom function `fmt_migrate()`](R/fmt_migrate.R) to make the formatting logic agnostic and flexible.
