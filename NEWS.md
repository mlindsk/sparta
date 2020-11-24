# sparta v0.6.1 (2020-11-24)

 * marginalization is now faster due to some refactoring

# sparta v0.6.0 (2020-11-09)

 * Prevent print from printing dimension names
 * A bug, due to wrong sorting, that manifested in both `marg`, `slice`, `mult` and `div` has been fixed. These functions were prone to errors when the number of variables exceed `10`.
 * It is now possible to multiply and divide sparta tables with a scalar where the scalar can be either of the two inputs. Prior to v0.6.0 the scalar had to be the second argument.

# sparta v0.5.0 (2020-10-08)

 * First release
