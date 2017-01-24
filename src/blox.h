
typedef void * ptr_t;            // for length
typedef char * string;           // string type
typedef int    bool;             // represent bool as an integer

typedef struct Block_Lit {       // not visible to user 
    bool *faces;                 // 6 block faces
} Block;

typedef struct Face_Lit {
    int *dim;                    // 3 dimensions
} Face_Lit;
typedef const Face_Lit *Face;    // FaceId is a literal

typedef struct Frame_Lit {
    int *dim;                    // instead of x,y,z
    Block *blocks;
    char *fr_id;
} Frame_Lit;
typedef const Frame_Lit *Frame;  // Frame is a literal

typedef struct Arr_Int {         // Array of integers
    size_t len;
    int* arr;
} Arr_Int;

typedef struct Arr_Flt {         // Array of integers
    size_t len;
    float *arr;
} Arr_Flt;

typedef struct Arr_Bool {        // Array of bools
    size_t len;
    bool *arr;
} Arr_Bool;

typedef struct Arr_Str {         // Array of strings
    size_t len;
    string *arr;
} Arr_Str;

typedef struct Arr_Frame_Lit {   // Array of Frames
    size_t len;
    Frame *arr;
} Arr_Frame_Lit;

typedef struct Arr_Face_Lit {    // Array of Faces
    size_t len;
    Face *arr;
} Arr_Face_Lit;
