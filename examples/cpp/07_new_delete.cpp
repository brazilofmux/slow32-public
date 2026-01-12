// Test 7: Dynamic allocation with new/delete
// Expected: Will FAIL - needs operator new/delete implementation

class Node {
public:
    int value;
    Node* next;

    Node(int v) : value(v), next(nullptr) {}
};

int main() {
    // Single object
    Node* n1 = new Node(10);
    Node* n2 = new Node(20);
    Node* n3 = new Node(30);

    n1->next = n2;
    n2->next = n3;

    // Sum linked list
    int sum = 0;
    for (Node* p = n1; p != nullptr; p = p->next) {
        sum += p->value;
    }

    // Cleanup
    delete n3;
    delete n2;
    delete n1;

    // Array new/delete
    int* arr = new int[5];
    for (int i = 0; i < 5; i++) {
        arr[i] = i * 10;
    }
    int arr_sum = arr[0] + arr[1] + arr[2] + arr[3] + arr[4];  // 0+10+20+30+40=100
    delete[] arr;

    return (sum == 60 && arr_sum == 100) ? 0 : 1;
}
