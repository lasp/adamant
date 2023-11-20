// C-lib data type
typedef struct {
    unsigned int count;
    unsigned int limit;
} c_data;

// C-lib function
unsigned int increment(c_data* data);
