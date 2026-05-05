/* second helper module — referenced indirectly so we exercise the
 * archive resolver having to pull two members. */

int double_it(int x);

int quadruple(int x) {
    return double_it(double_it(x));
}
