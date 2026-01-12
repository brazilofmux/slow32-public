// Test 5: Global object construction/destruction
// Expected: Will FAIL without .init_array support

class Logger {
    static int instance_count;
    int id;
public:
    Logger() : id(++instance_count) {}
    ~Logger() { instance_count--; }
    int get_id() const { return id; }
    static int count() { return instance_count; }
};

int Logger::instance_count = 0;

// Global objects - constructors must run before main()
Logger g_logger1;
Logger g_logger2;
Logger g_logger3;

int main() {
    // If constructors ran, count should be 3
    int count = Logger::count();

    // IDs should be 1, 2, 3
    int id1 = g_logger1.get_id();
    int id2 = g_logger2.get_id();
    int id3 = g_logger3.get_id();

    // Local object
    Logger local;
    int id4 = local.get_id();  // Should be 4
    int count2 = Logger::count();  // Should be 4

    return (count == 3 && id1 == 1 && id2 == 2 && id3 == 3 &&
            id4 == 4 && count2 == 4) ? 0 : 1;
}
