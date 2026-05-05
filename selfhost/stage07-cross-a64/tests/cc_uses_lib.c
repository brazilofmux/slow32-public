/* main program for the toolchain end-to-end test.
 * Calls quadruple(10) which calls double_it(double_it(10)) = 40. */

int quadruple(int x);

int main(void) {
    return quadruple(10);   /* expect exit code 40 */
}
