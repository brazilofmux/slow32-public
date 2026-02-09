/*
 * SLOW-32 CORDIC Math Library
 *
 * Implements transcendental functions using only additions, subtractions,
 * and shifts. Ideal for the SLOW-32 architecture where MUL is slow.
 */

#include <math.h>

/* CORDIC Gain Constants (Double Precision) */
#define CORDIC_K  0.60725293500888125617  /* Circular Gain */
#define CORDIC_KH 1.20749706776307212888  /* Hyperbolic Gain */

/* Table of atan(2^-i) for i = 0 to 52 */
static const double atan_table[] = {
    0.7853981633974483, 0.4636476090008061, 0.2449786631268641,
    0.1243549945467614, 0.0624188100023965, 0.0312398334302683,
    0.0156237286204768, 0.0078123410601011, 0.0039062301319670,
    0.0019531225164788, 0.0009765621895593, 0.0004882812111949,
    0.0002441406201494, 0.0001220703118937, 0.0000610351561742,
    0.0000305175781155, 0.0000152587890613, 0.0000076293945311,
    0.0000038146972656, 0.0000019073486328, 0.0000009536743164,
    0.0000004768371582, 0.0000002384185791, 0.0000001192092896,
    0.0000000596046448, 0.0000000298023224, 0.0000000149011612,
    0.0000000074505806, 0.0000000037252903, 0.0000000018626451,
    0.0000000009313226, 0.0000000004656613, 0.0000000002328306,
    0.0000000001164153, 0.0000000000582077, 0.0000000000291038,
    0.0000000000145519, 0.0000000000072760, 0.0000000000036380,
    0.0000000000018190, 0.0000000000009095, 0.0000000000004547,
    0.0000000000002274, 0.0000000000001137, 0.0000000000000568,
    0.0000000000000284, 0.0000000000000142, 0.0000000000000071,
    0.0000000000000036, 0.0000000000000018, 0.0000000000000009,
    0.0000000000000004, 0.0000000000000002
};

/* Table of atanh(2^-i) for i = 1 to 52 */
static const double atanh_table[] = {
    0.0, /* index 0 unused */
    0.5493061443340548, 0.2554128118829953, 0.1256572141424756,
    0.0625815714811865, 0.0312601784912111, 0.0156262717521115,
    0.0078126589533562, 0.0039063324339301, 0.0019531353041005,
    0.0009765637880110, 0.0004882814110013, 0.0002441406451252,
    0.0001220703150157, 0.0000610351565644, 0.0000305175781643,
    0.0000152587890674, 0.0000076293945319, 0.0000038146972657,
    0.0000019073486328, 0.0000009536743164, 0.0000004768371582,
    0.0000002384185791, 0.0000001192092896, 0.0000000596046448,
    0.0000000298023224, 0.0000000149011612, 0.0000000074505806,
    0.0000000037252903, 0.0000000018626451, 0.0000000009313226,
    0.0000000004656613, 0.0000000002328306, 0.0000000001164153,
    0.0000000000582077, 0.0000000000291038, 0.0000000000145519,
    0.0000000000072760, 0.0000000000036380, 0.0000000000018190,
    0.0000000000009095, 0.0000000000004547, 0.0000000000002274,
    0.0000000000001137, 0.0000000000000568, 0.0000000000000284,
    0.0000000000000142, 0.0000000000000071, 0.0000000000000036,
    0.0000000000000018, 0.0000000000000009, 0.0000000000000004,
    0.0000000000000002
};

/* Internal CORDIC Rotation (Circular)
 * Calculates: x' = K * (x*cos(z) - y*sin(z))
 *             y' = K * (y*cos(z) + x*sin(z))
 * If x=1, y=0, then x' = K*cos(z), y' = K*sin(z)
 */
void cordic_rotate_circular(double *x, double *y, double z) {
    double x_val = *x;
    double y_val = *y;
    double p2 = 1.0;

    for (int i = 0; i < 52; i++) {
        double x_next;
        if (z >= 0) {
            x_next = x_val - y_val * p2;
            y_val = y_val + x_val * p2;
            z -= atan_table[i];
        } else {
            x_next = x_val + y_val * p2;
            y_val = y_val - x_val * p2;
            z += atan_table[i];
        }
        x_val = x_next;
        p2 *= 0.5;
    }
    *x = x_val * CORDIC_K;
    *y = y_val * CORDIC_K;
}

/* Internal CORDIC Vectoring (Circular)
 * Calculates: z' = z + atan2(y, x)
 */
double cordic_vector_circular(double x, double y) {
    double z = 0.0;
    double p2 = 1.0;

    for (int i = 0; i < 52; i++) {
        double x_next;
        if (y < 0) {
            x_next = x, x_next += y * p2;
            y = y - x * p2;
            z -= atan_table[i];
        } else {
            x_next = x, x_next -= y * p2;
            y = y + x * p2;
            z += atan_table[i];
        }
        x = x_next;
        p2 *= 0.5;
    }
    return z;
}

/* Internal CORDIC Rotation (Hyperbolic)
 * Iterations 4, 13, 40, ... must be repeated
 */
void cordic_rotate_hyperbolic(double *x, double *y, double z) {
    double x_val = *x;
    double y_val = *y;
    double p2 = 0.5;
    int k = 4;

    for (int i = 1; i < 52; i++) {
        int repeat = (i == k);
        if (repeat) k = 3 * k + 1;

        do {
            double x_next;
            if (z >= 0) {
                x_next = x_val + y_val * p2;
                y_val = y_val + x_val * p2;
                z -= atanh_table[i];
            } else {
                x_next = x_val - y_val * p2;
                y_val = y_val - x_val * p2;
                z += atanh_table[i];
            }
            x_val = x_next;
            repeat--;
        } while (repeat >= 0);
        p2 *= 0.5;
    }
    *x = x_val * CORDIC_KH;
    *y = y_val * CORDIC_KH;
}

/* External wrappers for math_soft.c */

double cordic_sin(double z) {
    double x = 1.0, y = 0.0;
    /* Range reduction to [-pi/2, pi/2] */
    int quadrant = 0;
    if (z > M_PI_2 || z < -M_PI_2) {
        quadrant = (int)floor(z / M_PI_2 + 0.5);
        z -= (double)quadrant * M_PI_2;
    }
    cordic_rotate_circular(&x, &y, z);
    switch (quadrant & 3) {
        case 0: return y;
        case 1: return x;
        case 2: return -y;
        case 3: return -x;
    }
    return y;
}

double cordic_cos(double z) {
    double x = 1.0, y = 0.0;
    int quadrant = 0;
    if (z > M_PI_2 || z < -M_PI_2) {
        quadrant = (int)floor(z / M_PI_2 + 0.5);
        z -= (double)quadrant * M_PI_2;
    }
    cordic_rotate_circular(&x, &y, z);
    switch (quadrant & 3) {
        case 0: return x;
        case 1: return -y;
        case 2: return -x;
        case 3: return y;
    }
    return x;
}

double cordic_atan2(double y, double x) {
    if (x == 0.0 && y == 0.0) return 0.0;
    double z = 0.0;
    if (x < 0.0) {
        if (y >= 0.0) {
            z = M_PI;
            x = -x; y = -y;
        } else {
            z = -M_PI;
            x = -x; y = -y;
        }
    }
    return z + cordic_vector_circular(x, y);
}

double cordic_exp(double z) {
    double x = 1.0, y = 0.0;
    cordic_rotate_hyperbolic(&x, &y, z);
    return x + y;
}
