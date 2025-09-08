#include <math.h>

union float_bits {
    float f;
    unsigned int i;
};

union double_bits {
    double d;
    unsigned long long i;
};

double fabs(double x) {
    union double_bits u;
    u.d = x;
    u.i &= 0x7FFFFFFFFFFFFFFFULL;
    return u.d;
}

float fabsf(float x) {
    union float_bits u;
    u.f = x;
    u.i &= 0x7FFFFFFF;
    return u.f;
}

double sqrt(double x) {
    if (x < 0) return 0.0 / 0.0;
    if (x == 0) return 0;
    
    double guess = x;
    double epsilon = 1e-10;
    
    while (1) {
        double next = 0.5 * (guess + x / guess);
        if (fabs(next - guess) < epsilon) break;
        guess = next;
    }
    
    return guess;
}

float sqrtf(float x) {
    if (x < 0) return 0.0f / 0.0f;
    if (x == 0) return 0;
    
    float guess = x;
    float epsilon = 1e-6f;
    
    while (1) {
        float next = 0.5f * (guess + x / guess);
        if (fabsf(next - guess) < epsilon) break;
        guess = next;
    }
    
    return guess;
}

double exp(double x) {
    double sum = 1.0;
    double term = 1.0;
    int n = 1;
    
    while (fabs(term) > 1e-15 && n < 100) {
        term *= x / n;
        sum += term;
        n++;
    }
    
    return sum;
}

float expf(float x) {
    float sum = 1.0f;
    float term = 1.0f;
    int n = 1;
    
    while (fabsf(term) > 1e-7f && n < 50) {
        term *= x / n;
        sum += term;
        n++;
    }
    
    return sum;
}

double log(double x) {
    if (x <= 0) return 0.0 / 0.0;
    if (x == 1) return 0;
    
    double y = (x - 1) / (x + 1);
    double y2 = y * y;
    double sum = 0;
    double term = y;
    int n = 1;
    
    while (fabs(term) > 1e-15 && n < 100) {
        sum += term / n;
        term *= y2;
        n += 2;
    }
    
    return 2 * sum;
}

float logf(float x) {
    return (float)log(x);
}

double log10(double x) {
    return log(x) / M_LN10;
}

float log10f(float x) {
    return logf(x) / (float)M_LN10;
}

double pow(double x, double y) {
    if (x == 0) return (y == 0) ? 1 : 0;
    if (y == 0) return 1;
    if (y == 1) return x;
    
    if (y == (int)y) {
        double result = 1;
        int n = (int)y;
        if (n < 0) {
            x = 1 / x;
            n = -n;
        }
        while (n) {
            if (n & 1) result *= x;
            x *= x;
            n >>= 1;
        }
        return result;
    }
    
    return exp(y * log(x));
}

float powf(float x, float y) {
    return (float)pow(x, y);
}

double sin(double x) {
    while (x > M_PI) x -= 2 * M_PI;
    while (x < -M_PI) x += 2 * M_PI;
    
    double sum = x;
    double term = x;
    int n = 1;
    
    while (fabs(term) > 1e-15 && n < 20) {
        term *= -x * x / ((2 * n) * (2 * n + 1));
        sum += term;
        n++;
    }
    
    return sum;
}

float sinf(float x) {
    while (x > M_PI) x -= 2 * M_PI;
    while (x < -M_PI) x += 2 * M_PI;
    
    float sum = x;
    float term = x;
    int n = 1;
    
    while (fabsf(term) > 1e-7f && n < 10) {
        term *= -x * x / ((2 * n) * (2 * n + 1));
        sum += term;
        n++;
    }
    
    return sum;
}

double cos(double x) {
    while (x > M_PI) x -= 2 * M_PI;
    while (x < -M_PI) x += 2 * M_PI;
    
    double sum = 1;
    double term = 1;
    int n = 1;
    
    while (fabs(term) > 1e-15 && n < 20) {
        term *= -x * x / ((2 * n - 1) * (2 * n));
        sum += term;
        n++;
    }
    
    return sum;
}

float cosf(float x) {
    while (x > M_PI) x -= 2 * M_PI;
    while (x < -M_PI) x += 2 * M_PI;
    
    float sum = 1;
    float term = 1;
    int n = 1;
    
    while (fabsf(term) > 1e-7f && n < 10) {
        term *= -x * x / ((2 * n - 1) * (2 * n));
        sum += term;
        n++;
    }
    
    return sum;
}

double tan(double x) {
    double c = cos(x);
    if (fabs(c) < 1e-15) return 1e15 * ((sin(x) > 0) ? 1 : -1);
    return sin(x) / c;
}

float tanf(float x) {
    float c = cosf(x);
    if (fabsf(c) < 1e-7f) return 1e7f * ((sinf(x) > 0) ? 1 : -1);
    return sinf(x) / c;
}

double atan(double x) {
    if (fabs(x) > 1) {
        return ((x > 0) ? M_PI_2 : -M_PI_2) - atan(1 / x);
    }
    
    double sum = x;
    double term = x;
    double x2 = x * x;
    int n = 1;
    
    while (fabs(term) > 1e-15 && n < 100) {
        term *= -x2 * (2 * n - 1) / (2 * n + 1);
        sum += term / (2 * n + 1);
        n++;
    }
    
    return sum;
}

float atanf(float x) {
    return (float)atan(x);
}

double atan2(double y, double x) {
    if (x > 0) return atan(y / x);
    if (x < 0 && y >= 0) return atan(y / x) + M_PI;
    if (x < 0 && y < 0) return atan(y / x) - M_PI;
    if (x == 0 && y > 0) return M_PI_2;
    if (x == 0 && y < 0) return -M_PI_2;
    return 0;
}

float atan2f(float y, float x) {
    return (float)atan2(y, x);
}

double floor(double x) {
    long long i = (long long)x;
    return (x < i) ? i - 1 : i;
}

float floorf(float x) {
    int i = (int)x;
    return (x < i) ? i - 1 : i;
}

double ceil(double x) {
    long long i = (long long)x;
    return (x > i) ? i + 1 : i;
}

float ceilf(float x) {
    int i = (int)x;
    return (x > i) ? i + 1 : i;
}

double round(double x) {
    return (x >= 0) ? floor(x + 0.5) : ceil(x - 0.5);
}

float roundf(float x) {
    return (x >= 0) ? floorf(x + 0.5f) : ceilf(x - 0.5f);
}

double trunc(double x) {
    return (x >= 0) ? floor(x) : ceil(x);
}

float truncf(float x) {
    return (x >= 0) ? floorf(x) : ceilf(x);
}

double fmod(double x, double y) {
    if (y == 0) return 0.0;
    // Simplified without division
    while (x >= y) x -= y;
    while (x < 0) x += y;
    return x;
}

float fmodf(float x, float y) {
    if (y == 0) return 0.0f;
    // Simplified without division
    while (x >= y) x -= y;
    while (x < 0) x += y;
    return x;
}