/* Recursive function with a fused compare in the base case and a
   second use of the parameter after the recursive calls. Both
   pre-recursion live range and post-recursion live range need to
   survive the fused BRC. */
int fib(int n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int main(void) {
    return fib(10) - 55;
}
