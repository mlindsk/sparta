# sparta v0.8.1.1 (2021-09-30)

 * Bug fixed when mult/div two unities
 * Bug fixed when calling `div(x,y)` with `x` being a scalar. The result was the same as `div(y,x)`.

# sparta v0.8.1 (2021-07-05)

 * `as_sparta.data.frame` does no longer convert to an array first (expensive) and is therfore much more efficient now.

# sparta v0.8.0 (2021-06-18)

 * `marg` is now more efficient when `y` is `character(0)`
 * A `sparta_unity_struct` can now be sliced if `drop ` TRUE=.
 * Better argument checks in `slice`
 * `mult` and `div` is now faster when one of the tables is a subtale of the other. This is especially useful in connection with Bayesian networks and the Junction Tree Algorithm.
 * `sum, normalize, get_vals` and `vals` now works as expected on `sparta_unity` class
 * New functions: `sparta_rank` and `table_size`
 * `as_array` now works for `sparta_unity`

# sparta v0.7.3 (2021-04-22)

 * Bug fixed when argument `drop` is `TRUE` in `slice` for tables that should reduce to one row.

# sparta v0.7.2 (2021-04-03)
 * The new print method turned out to cause problems in the long run. Printing a sparse table now prints columns as rows again with the corresponding value attached to the right.
 * marginalizing is much more memory efficient now. Especially for massive tables.

# sparta v0.7.1.0 (2021-03-01)

 * Multiplication of unity tables was not correct due to the new rank argument. The fix was to multiply the rank of the tables.
 * Print method changed to print cells as columns which is also how the sparse tables are represented under the hood.
 * New functions (see documentation):
   + `sparsity`: determines the ratio of non-zero cells and the size of the statespace.
   + `as_df`: convert a sparse table to a `data.frame`. Either with or without zero cells
   + `get_values`: The same as the existing `vals`

# sparta v0.7.0 (2020-12-16)

 * unity tables now have a rank attribute which is just the
 unique value that all cells have. Because of this, a unity
 table can now be multiplied with a scalar. And regular 
 sparta tables can be multiplied with unities of different rank.
 As a consequence, unities can now be marginalized also.
 
 * New function `equiv` to test if two sparta objects are identical
 
 * Fixed a bug when assigning dimnames to the result of multiplication

# sparta v0.6.1 (2020-11-24)

 * marginalization is now faster due to some refactoring

# sparta v0.6.0 (2020-11-09)

 * Prevent print from printing dimension names
 * A bug, due to wrong sorting, that manifested in both `marg`, `slice`, `mult` and `div` has been fixed. These functions were prone to errors when the number of variables exceed `10`.
 * It is now possible to multiply and divide sparta tables with a scalar where the scalar can be either of the two inputs. Prior to v0.6.0 the scalar had to be the second argument.

# sparta v0.5.0 (2020-10-08)

 * First release
