/* polymorphism/fake_graphics2.cpp */
#include <stdio.h>

// To fix OOP, let us introduce a logical commong parent to Pixel and Circle:
// GraphObject, an abstract (i. e. not to be used as is) class with purely
// virtual methods (without implementation in the base class)

// A class with at least one purely virt funct is considered abstract and
// the compiler does not allow to create objects of this class
// If a child does not implement all purely virt funcs, it is also considered
// abstract
class GraphObject {
protected:
    float x, y;
    int color;

public:
    // Note on virtual consrtuctors/destructors:
    // The constructor first constucts all sub-objects ( : a(a), ...),
    // including base class, then the vmtp gets filled out, and then the
    // body of the constructor.
    // Thus, inside the constructor body any called vfunc will use the 
    // implementation of the class, which owns the constructor (since it
    // has just put it's own vtable pointer in vmtp). Thus vfuncs will
    // not work as usual in constructors.
    // Same with destructors -- it deinitialisez vmtp back down the 
    // destructor stack
    GraphObject(float ax, float ay, int acolor)
        : x(ax), y(ay), color(acolor) {}
    virtual ~GraphObject() {}

    // = 0 indicates purely virt func (something like funcptr = 0)
    // They do not have to be implemented, and will appear in vtables
    virtual void Show() = 0;
    virtual void Hide() = 0;

    void Move(float nx, float ny);
};

void GraphObject::Move(float nx, float ny)
{
    Hide();
    x = nx;
    y = ny;
    Show();
}

// Now we can fix the hierarchy to be logical

class Pixel : public GraphObject {
public:
    Pixel(float ax, float ay, int acolor)
        : GraphObject(ax, ay, acolor) {}
    virtual ~Pixel() {}

    virtual void Show();
    virtual void Hide();
};

class Circle : public GraphObject {
    float radius;

public:
    Circle(float ax, float ay, float aradius, int acolor)
        : GraphObject(ax, ay, acolor), radius(aradius) {}
    virtual ~Circle() {}

    virtual void Show();
    virtual void Hide();
};

void Pixel::Show()
{
    printf("Show pixel at (%.2f, %.2f) with color 0x%X\n", x, y, color);
}

void Pixel::Hide()
{
    printf("Hide pixel at (%.2f, %.2f) with color 0x%X\n", x, y, color);
}

void Circle::Show()
{
    printf("Show circle at (%.2f, %.2f) with raduis %f and color 0x%X\n", 
           x, y, radius, color);
}

void Circle::Hide()
{
    printf("Hide circle at (%.2f, %.2f) with raduis %f and color 0x%X\n", 
           x, y, radius, color);
}

class PolygonalChain : public GraphObject {
    struct vertex_t {
        float dx, dy;
        vertex_t *next;
    };
    vertex_t *first;

public:
    PolygonalChain(float ax, float ay, int acolor)
        : GraphObject(ax, ay, acolor), first(NULL) {}
    virtual ~PolygonalChain();

    void AddVertex(float adx, float ady);

    virtual void Show();
    virtual void Hide();
};

PolygonalChain::~PolygonalChain()
{
    while (first) {
        vertex_t *tmp = first;
        first = first->next;
        delete tmp;
    }
}

void PolygonalChain::AddVertex(float adx, float ady)
{
    vertex_t *tmp = new vertex_t;
    tmp->dx = adx;
    tmp->dy = ady;
    tmp->next = first;
    first = tmp;
}

void PolygonalChain::Show()
{
    printf("Show polychain at (%.2f, %.2f) with color 0x%X\n", x, y, color);
    printf("Vertices:");
    for (vertex_t *tmp = first; tmp; tmp = tmp->next)
        printf(" (%.2f, %.2f)", x + tmp->dx, y + tmp->dy);
    putchar('\n');
}

void PolygonalChain::Hide()
{
    printf("Hide polychain at (%.2f, %.2f) with color 0x%X\n", x, y, color);
    printf("Vertices:");
    for (vertex_t *tmp = first; tmp; tmp = tmp->next)
        printf(" (%.2f, %.2f)", x + tmp->dx, y + tmp->dy);
    putchar('\n');
}

// Here is a use case for inheritance: inheritance for constructor. This is
// done to get essentially the same object, but with certain defaults
class Square : public PolygonalChain {
public:
    Square(float ax, float ay, int acolor, float a)
        : PolygonalChain(ax, ay, acolor)
    {
        AddVertex(0, 0);
        AddVertex(0, a);
        AddVertex(a, a);
        AddVertex(a, 0);
        AddVertex(0, 0);
    }
};

int main()
{
    Pixel p(0, 0, 0x230301);
    Circle c(4, 5, 1, 0xFF00FF);
    p.Move(102.6, -44.4);
    putchar('\n');
    c.Move(0.3, 1.6);
    putchar('\n');

    // Here is a demonstration of the use of virt destructors:
    // Square uses allocations, thus we want it's destructor to be called
    // regardless of the pointer type
    GraphObject *ptr = new Square(27.3, 37.7, 0xff0000, 10.0);
    ptr->Move(17.3, 27.7);
    putchar('\n');
    delete ptr;

    // We can now define a scene with different objects, and not have a 
    // care in the world (while performance is ok)
    // Such vtable-runtime stuff is called dynamic polymorphism
    const int scene_size = 3;
    GraphObject **scene = new GraphObject *[scene_size];

    scene[0] = new Pixel(1.25, 15.75, 0xff0000);
    scene[1] = new Circle(20.9, 7.25, 0x005500, 3.5);
    scene[2] = new Square(55.0, 30.05, 0x008080, 10.0);

    for (int i = 0; i < scene_size; i++) {
        scene[i]->Show();
        delete scene[i];
    }
    delete [] scene;

    return 0;
}
